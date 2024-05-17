import Principal "mo:base/Principal";
import Text "mo:base/Text";
import HashMap "mo:base/HashMap";
import Nat "mo:base/Nat";
import Utils "./utils";
import Prelude "mo:base/Prelude";
import Buffer "mo:base/Buffer";
import sonicTypes "sonicTypes";
import Time "mo:base/Time";
import Iter "mo:base/Iter";

/**
 * An actor responsible for transferring tokens between different token canisters.
 */
actor Appic_Multiswap {

  /**
   * Defines the structure of a transfer receipt.
   */
  type TransferReceipt = {
    #Ok : Nat; // Indicates a successful transfer with a transaction ID
    #Err : Text; // Indicates an error occurred during the transfer
  };

  type TokenToNum = {
    tokenId : Principal;
    tokenNum : Nat;
  };

  type sonicActor = sonicTypes.sonicActor;
  type TxReceipt = sonicTypes.TxReceipt;

  public type TokenActor = actor {
    allowance : shared (owner : Principal, spender : Principal) -> async Nat;
    approve : shared (spender : Principal, value : Nat) -> async TransferReceipt;
    balanceOf : (owner : Principal) -> async Nat;
    decimals : () -> async Nat8;
    name : () -> async Text;
    symbol : () -> async Text;
    totalSupply : () -> async Nat;
    transfer : shared (to : Principal, value : Nat) -> async TransferReceipt;
    transferFrom : shared (from : Principal, to : Principal, value : Nat) -> async TransferReceipt;
  };

  public type ICRC1TokenActor = actor {
    icrc1_balance_of : (account : ICRCAccount) -> async Nat;
    icrc1_decimals : () -> async Nat8;
    icrc1_name : () -> async Text;
    icrc1_symbol : () -> async Text;
    icrc1_total_supply : () -> async Nat;
    icrc1_transfer : shared (ICRCTransferArg) -> async TransferReceipt;
  };
  public type ICRC2TokenActor = actor {
    icrc2_approve : shared (from_subaccount : ?Subaccount, spender : Principal, amount : Nat) -> async TransferReceipt;
    icrc2_allowance : shared (account : Subaccount, spender : Principal) -> async (allowance : Nat, expires_at : ?Nat64);
    icrc1_balance_of : (account : ICRCAccount) -> async Nat;
    icrc1_decimals : () -> async Nat8;
    icrc1_name : () -> async Text;
    icrc1_symbol : () -> async Text;
    icrc1_total_supply : () -> async Nat;
    icrc2_transfer_from : shared (ICRC2TransferArg) -> async TransferReceipt;
    icrc1_transfer : shared (ICRCTransferArg) -> async TransferReceipt;
  };
  type TokenActorVariable = {
    #DIPtokenActor : TokenActor;
    #ICRC1TokenActor : ICRC1TokenActor;
    #ICRC2TokenActor : ICRC2TokenActor;
  };

  type TxReceiptSonic = sonicTypes.TxReceipt;

  type Subaccount = Blob;
  type ICRCAccount = {
    owner : Principal;
    subaccount : ?Subaccount;
  };
  type ICRCTransferArg = {
    from_subaccount : ?Subaccount;
    to : ICRCAccount;
    amount : Nat;
  };
  type ICRC2TransferArg = {
    from : ICRCAccount;
    to : ICRCAccount;
    amount : Nat;
  };

  var userTokensLocked : HashMap.HashMap<Principal, HashMap.HashMap<Principal, Nat>> = HashMap.HashMap<Principal, HashMap.HashMap<Principal, Nat>>(1, Principal.equal, Principal.hash);
  private stable var txcounter : Nat = 0;
  let sonicCanisterId : Principal = Principal.fromText("3xwpq-ziaaa-aaaah-qcn4a-cai");
  let sonicCanister : sonicActor = sonicTypes._getSonicActor(sonicCanisterId); // Sonic canister

  private query func _getTokenActorWithType(tokenId : Text, tokenType : Text) : async TokenActorVariable {
    switch (tokenType) {
      case ("DIP20") {
        var tokenCanister : TokenActor = actor (tokenId);
        return #DIPtokenActor(tokenCanister);
      };
      case ("ICRC1") {
        var tokenCanister : ICRC1TokenActor = actor (tokenId);
        return #ICRC1TokenActor(tokenCanister);
      };
      case ("ICRC2") {
        var tokenCanister : ICRC2TokenActor = actor (tokenId);
        return #ICRC2TokenActor(tokenCanister);
      };
      case (_) {
        Prelude.unreachable();
      };
    };
  };

  private func _transferFrom(tokenId : Text, tokenType : Text, caller : Principal, value : Nat) : async TransferReceipt {
    var tokenCanister : TokenActorVariable = await _getTokenActorWithType(tokenId, tokenType);
    switch (tokenCanister) {
      case (#DIPtokenActor(dipTokenActor)) {
        var txid = await dipTokenActor.transferFrom(caller, Principal.fromActor(Appic_Multiswap), value);
        switch (txid) {
          case (#Ok(id)) { return #Ok(id) };
          case (#Err(e)) { return #Err(e) };
        };
      };
      case (#ICRC1TokenActor(icrc1TokenActor)) {

        let subaccount = Utils.defaultSubAccount();
        var defaultSubaccount : Blob = Utils.defaultSubAccount();
        var transferArg : ICRCTransferArg = {
          from_subaccount = ?subaccount;
          to = {
            owner = Principal.fromActor(Appic_Multiswap);
            subaccount = ?defaultSubaccount;
          };
          amount = value;
        };
        var txid = await icrc1TokenActor.icrc1_transfer(transferArg);
        switch (txid) {
          case (#Ok(id)) { return #Ok(id) };
          case (#Err(e)) { return #Err(e) };
        };
      };
      case (#ICRC2TokenActor(icrc2TokenActor)) {
        var transferArg = {
          from = { owner = caller; subaccount = null };
          to = {
            owner = Principal.fromActor(Appic_Multiswap);
            subaccount = null;
          };
          amount = value;
        };
        var txid = await icrc2TokenActor.icrc2_transfer_from(transferArg);
        switch (txid) {
          case (#Ok(id)) { return #Ok(id) };
          case (#Err(e)) { return #Err(e) };
        };
      };

    };
  };

  private func _transfer(tokenId : Text, tokenType : Text, caller : Principal, value : Nat) : async TransferReceipt {
    var tokenCanister : TokenActorVariable = await _getTokenActorWithType(tokenId, tokenType);
    switch (tokenCanister) {
      case (#DIPtokenActor(dipTokenActor)) {
        var txid = await dipTokenActor.transfer(caller, value);
        switch (txid) {
          case (#Ok(id)) { return #Ok(id) };
          case (#Err(e)) { return #Err(e) };
        };
      };
      case (#ICRC1TokenActor(icrc1TokenActor)) {
        var defaultSubaccount : Blob = Utils.defaultSubAccount();
        var transferArg : ICRCTransferArg = {
          from_subaccount = ?defaultSubaccount;
          to = { owner = caller; subaccount = ?defaultSubaccount };
          amount = value;
        };
        var txid = await icrc1TokenActor.icrc1_transfer(transferArg);
        switch (txid) {
          case (#Ok(id)) { return #Ok(id) };
          case (#Err(e)) { return #Err(e) };
        };
      };
      case (#ICRC2TokenActor(icrc2TokenActor)) {
        var defaultSubaccount : Blob = Utils.defaultSubAccount();
        var transferArg : ICRCTransferArg = {
          from_subaccount = ?defaultSubaccount;
          to = { owner = caller; subaccount = ?defaultSubaccount };
          amount = value;
        };
        var txid = await icrc2TokenActor.icrc1_transfer(transferArg);
        switch (txid) {
          case (#Ok(id)) { return #Ok(id) };
          case (#Err(e)) { return #Err(e) };
        };
      };

    };
  };

  private func swapTokensWithSonic(
    sellToken : Text,
    buyToken : Text,
    to : Principal,
    swapAmount : Nat,
  ) : async TxReceipt {
    let swapResult : TxReceipt = await sonicCanister.swapExactTokensForTokens(swapAmount, 0, [sellToken, buyToken], to, Time.now() + 300000);
    return swapResult;
  };

  /**
   * Transfers tokens from the caller to a specified token canister.
   * @param tokenCanister The target token canister.
   * @param caller The caller initiating the transfer.
   * @param value The amount of tokens to transfer.
   * @param tokenID The ID of the token to transfer.
   */
  private func transferTokensToCanister(tokenId : Principal, tokenType : Text, caller : Principal, value : Nat) : async TransferReceipt {

    // Retrieve user token data or initialize if null
    let _ = switch (await _transferFrom(Principal.toText(tokenId), tokenType, caller, value)) {
      case (#Ok(id)) { id };
      case (#Err(e)) { return #Err("token transfer failed") };
    };

    var userData : HashMap.HashMap<Principal, Nat> = HashMap.HashMap<Principal, Nat>(1, Principal.equal, Principal.hash);
    switch (userTokensLocked.get(caller)) {
      case (null) {
        // Handle the case where the value is null
        // userData := HashMap.HashMap<Principal, Nat>();
      };
      case (?value) {
        // Handle the case where the value is not null
        userData := value;
      };
    };

    // Update user balance with transferred tokens
    let newBalance = switch (userData.get(tokenId)) {
      case (null) { value };
      case (?current) { current + value };
    };
    switch (userData.get(tokenId)) {
      case (null) {
        userData.put(tokenId, newBalance);
      };
      case (?value) {
        let _ = userData.replace(tokenId, newBalance);
      };
    };

    // Update user token data
    let _ = userTokensLocked.replace(caller, userData);
    txcounter += 1;
    return #Ok(txcounter -1);
  };

  public func withdrawTokens(tokenType : Text, caller : Principal, tokenID : Principal) : async TransferReceipt {
    var userData : HashMap.HashMap<Principal, Nat> = switch (userTokensLocked.get(caller)) {
      case (null) {
        // Handle the case where the value is null
        // For example, you could return an error indicating that the caller has no tokens locked
        return #Err("No tokens locked for the caller");
      };
      case (?value) {
        // Handle the case where the value is not null
        value;
      };
    };
    let userBalance = switch (userData.get(tokenID)) {
      case (null) {
        return #Err("No tokens of the specified ID locked for the caller");
      };
      case (?balance) { balance };
    };

    let _ = switch (await _transfer(Principal.toText(tokenID), tokenType, caller, userBalance)) {
      case (#Ok(id)) { id };
      case (#Err(e)) { return #Err(e) };
    };

    userData.put(tokenID, 0); // Set the user's token balance to zero after withdrawal
    let _ = userTokensLocked.replace(caller, userData);
    txcounter += 1;
    return #Ok(txcounter -1);
  };

  public func Multiswap(sellingTokens : [Principal], buyingTokens : [Principal], sellAmounts : [Nat], buyAmounts : [Nat], midToken : Principal, midTokenType : Text, sellingTokensType : [Text], buyingTokensType : [Text], caller : Principal) {
    assert (sellingTokens.size() == sellAmounts.size());
    assert (buyingTokens.size() == buyAmounts.size());

    let midTokenActor : TokenActorVariable = await _getTokenActorWithType(Principal.toText(midToken), midTokenType);
    var midTokenBalInit = 0;
    switch (midTokenActor) {
      case (#DIPtokenActor(dipTokenActor)) {
        midTokenBalInit := await dipTokenActor.balanceOf(Principal.fromActor(Appic_Multiswap));
      };
      case (#ICRC1TokenActor(icrc1TokenActor)) {
        let accountAppic : ICRCAccount = {
          owner = Principal.fromActor(Appic_Multiswap);
          subaccount = null;
        };
        midTokenBalInit := await icrc1TokenActor.icrc1_balance_of(accountAppic);
      };
      case (#ICRC2TokenActor(icrc2TokenActor)) {
        let accountAppic : ICRCAccount = {
          owner = Principal.fromActor(Appic_Multiswap);
          subaccount = null;
        };
        midTokenBalInit := await icrc2TokenActor.icrc1_balance_of(accountAppic);
      };
    };

    for (i in Iter.range(0, sellingTokens.size() -1)) {
      let _ = switch (await transferTokensToCanister(sellingTokens[i], sellingTokensType[i], caller, sellAmounts[i])) {
        case (#Ok(id)) { #Ok(id) };
        case (#Err(e)) { #Err("token transfer failed:") };
      };
      let tokenActor : TokenActorVariable = await _getTokenActorWithType(Principal.toText(sellingTokens[i]), sellingTokensType[i]);
      switch (tokenActor) {
        case (#DIPtokenActor(dipTokenActor)) {
          let _ = switch (await dipTokenActor.approve(sonicCanisterId, sellAmounts[i])) {
            case (#Ok(id)) { #Ok(id) };
            case (#Err(e)) { #Err("token transfer failed:") };
          };
          let _ = await sonicCanister.deposit(sellingTokens[i], sellAmounts[i]);
          let _ = await swapTokensWithSonic(Principal.toText(sellingTokens[i]), Principal.toText(midToken), Principal.fromActor(Appic_Multiswap), sellAmounts[i]);
        };
        case (#ICRC1TokenActor(icrc1TokenActor)) {
          let _ = await sonicCanister.deposit(sellingTokens[i], sellAmounts[i]);
          let _ = await swapTokensWithSonic(Principal.toText(sellingTokens[i]), Principal.toText(midToken), Principal.fromActor(Appic_Multiswap), sellAmounts[i]);

        };
        case (#ICRC2TokenActor(icrc2TokenActor)) {
          let _ = switch (await icrc2TokenActor.icrc2_approve(null, sonicCanisterId, sellAmounts[i])) {
            case (#Ok(id)) { #Ok(id) };
            case (#Err(e)) { #Err("token transfer failed:") };
          };
          let _ = await sonicCanister.deposit(sellingTokens[i], sellAmounts[i]);
          let _ = await swapTokensWithSonic(Principal.toText(sellingTokens[i]), Principal.toText(midToken), Principal.fromActor(Appic_Multiswap), sellAmounts[i]);
        };
      };
    };

    var midTokenBalFin = 0;
    switch (midTokenActor) {
      case (#DIPtokenActor(dipTokenActor)) {
        midTokenBalFin := await dipTokenActor.balanceOf(Principal.fromActor(Appic_Multiswap));
      };
      case (#ICRC1TokenActor(icrc1TokenActor)) {
        let accountAppic : ICRCAccount = {
          owner = Principal.fromActor(Appic_Multiswap);
          subaccount = null;
        };
        midTokenBalFin := await icrc1TokenActor.icrc1_balance_of(accountAppic);
      };
      case (#ICRC2TokenActor(icrc2TokenActor)) {
        let accountAppic : ICRCAccount = {
          owner = Principal.fromActor(Appic_Multiswap);
          subaccount = null;
        };
        midTokenBalFin := await icrc2TokenActor.icrc1_balance_of(accountAppic);
      };
    };

    let midTokenBal = midTokenBalFin -midTokenBalInit;

    for (i in Iter.range(0, buyingTokens.size() -1)) {
      let tokenActor : TokenActorVariable = await _getTokenActorWithType(Principal.toText(buyingTokens[i]), buyingTokensType[i]);
      let buyActulAmt = buyAmounts[i] * midTokenBal / 100;
      switch (tokenActor) {
        case (#DIPtokenActor(dipTokenActor)) {
          let _ = switch (await dipTokenActor.approve(sonicCanisterId, buyActulAmt[i])) {
            case (#Ok(id)) { #Ok(id) };
            case (#Err(e)) { #Err("token transfer failed:") };
          };
          let _ = await sonicCanister.deposit(buyingTokens[i], buyActulAmt[i]);
          let _ = await swapTokensWithSonic(Principal.toText(buyingTokens[i]), Principal.toText(midToken), Principal.fromActor(Appic_Multiswap), buyActulAmt[i]);
        };
        case (#ICRC1TokenActor(icrc1TokenActor)) {
          let _ = await sonicCanister.deposit(buyingTokens[i], buyActulAmt[i]);
          let _ = await swapTokensWithSonic(Principal.toText(buyingTokens[i]), Principal.toText(midToken), Principal.fromActor(Appic_Multiswap), buyActulAmt[i]);

        };
        case (#ICRC2TokenActor(icrc2TokenActor)) {
          let _ = switch (await icrc2TokenActor.icrc2_approve(null, sonicCanisterId, buyActulAmt[i])) {
            case (#Ok(id)) { #Ok(id) };
            case (#Err(e)) { #Err("token transfer failed:") };
          };
          let _ = await sonicCanister.deposit(buyingTokens[i], buyActulAmt[i]);
          let _ = await swapTokensWithSonic(Principal.toText(buyingTokens[i]), Principal.toText(midToken), caller, buyActulAmt[i]);
        };
      };
    };

  };

  public query func getAllUserTokens(caller : Principal) : async ([Principal], [Nat]) {
    switch (userTokensLocked.get(caller)) {
      case (null) {
        // If no data found, return an empty hash map
        return ([], []);
      };

      case (?userData) {
        let tokensNum = userData.size();
        var arrayTokens = Buffer.Buffer<Principal>(tokensNum);
        var arrayNums = Buffer.Buffer<Nat>(tokensNum);
        for ((key, value) in userData.entries()) {
          arrayTokens.add(key);
          arrayNums.add(value);
        };
        return (Buffer.toArray(arrayTokens), Buffer.toArray(arrayNums));
      };
    };
  };

};
