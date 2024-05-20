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
import Blob "mo:base/Blob";

actor Appic_Multiswap {

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

  private stable var txcounter : Nat = 0;
  let sonicCanisterId : Principal = Principal.fromText("3xwpq-ziaaa-aaaah-qcn4a-cai");
  let sonicCanister : sonicActor = sonicTypes._getSonicActor(sonicCanisterId); // Sonic canister

  /**
   * @notice Get the token actor with its type
   * @dev This function returns the token actor variable based on the token type
   * @param tokenId The ID of the token
   * @param tokenType The type of the token (e.g., "DIP20", "ICRC1", "ICRC2")
   * @return The token actor variable
   */
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

  /**
   * @notice Transfer tokens from a caller to this actor
   * @dev This function handles transferring tokens from the caller to this actor based on token type
   * @param tokenId The ID of the token
   * @param tokenType The type of the token (e.g., "DIP20", "ICRC1", "ICRC2")
   * @param caller The principal of the caller
   * @param value The amount of tokens to transfer
   * @return Transfer receipt indicating success or error
   */
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
        var defaultSubaccount : Blob = Utils.defaultSubAccount();
        var userSubAccount : Subaccount = getSubAccount();
        var transferArg : ICRCTransferArg = {
          from_subaccount = ?userSubAccount;
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

  /**
   * @notice Transfer tokens to a caller
   * @dev This function handles transferring tokens to the caller based on token type
   * @param tokenId The ID of the token
   * @param tokenType The type of the token (e.g., "DIP20", "ICRC1", "ICRC2")
   * @param caller The principal of the caller
   * @param value The amount of tokens to transfer
   * @return Transfer receipt indicating success or error
   */
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

  /**
 * @notice Swaps tokens using Sonic canister
 * @dev This function swaps an exact amount of `sellToken` for `buyToken` using the Sonic canister.
 * @param sellToken The token to sell
 * @param buyToken The token to buy
 * @param to The recipient principal of the bought tokens
 * @param swapAmount The amount of `sellToken` to swap
 * @return The transaction receipt indicating success or error
 */
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
 * @notice Transfers tokens to this canister
 * @dev This function transfers tokens from the caller to this canister and updates the user's locked token data.
 * @param tokenId The ID of the token
 * @param tokenType The type of the token (e.g., "DIP20", "ICRC1", "ICRC2")
 * @param caller The principal of the caller
 * @param value The amount of tokens to transfer
 * @return Transfer receipt indicating success or error
 */
  private func transferTokensToCanister(tokenId : Principal, tokenType : Text, caller : Principal, value : Nat) : async TransferReceipt {

    // Retrieve user token data or initialize if null
    let _ = switch (await _transferFrom(Principal.toText(tokenId), tokenType, caller, value)) {
      case (#Ok(id)) { id };
      case (#Err(e)) { return #Err("token transfer failed") };
    };
    txcounter += 1;
    return #Ok(txcounter -1);
  };

  /**
 * @notice Withdraws tokens to the caller
 * @dev This function allows the caller to withdraw their locked tokens.
 * @param tokenType The type of the token (e.g., "DIP20", "ICRC1", "ICRC2")
 * @param caller The principal of the caller
 * @param tokenID The ID of the token to withdraw
 * @return Transfer receipt indicating success or error
 */
  public func withdrawTokens(tokenType : Text, caller : Principal, tokenID : Principal, userBalance : Nat) : async TransferReceipt {
    let _ = switch (await _transfer(Principal.toText(tokenID), tokenType, caller, userBalance)) {
      case (#Ok(id)) { id };
      case (#Err(e)) { return #Err(e) };
    };

    txcounter += 1;
    return #Ok(txcounter -1);
  };

  /**
 * @notice Performs a multi-token swap
 * @dev This function handles swapping multiple types of tokens through an intermediate token.
 * @param sellingTokens Array of token IDs to sell
 * @param buyingTokens Array of token IDs to buy
 * @param sellAmounts Array of amounts of each token to sell
 * @param buyAmounts Array of amounts of each token to buy
 * @param midToken The intermediate token used for swapping
 * @param midTokenType The type of the intermediate token (e.g., "DIP20", "ICRC1", "ICRC2")
 * @param sellingTokensType Array of types of tokens to sell
 * @param buyingTokensType Array of types of tokens to buy
 * @param caller The principal of the caller
 */
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
      // transfer tokens to canister
      let _ = switch (await transferTokensToCanister(sellingTokens[i], sellingTokensType[i], caller, sellAmounts[i])) {
        case (#Ok(id)) { #Ok(id) };
        case (#Err(e)) { #Err("token transfer failed:") };
      };
      let tokenActor : TokenActorVariable = await _getTokenActorWithType(Principal.toText(sellingTokens[i]), sellingTokensType[i]);
      let sonicBalanceOfBuyTokenBeforeTrade = await sonicCanister.balanceOf(Principal.toText(midToken), Principal.fromActor(Appic_Multiswap));
      switch (tokenActor) {
        case (#DIPtokenActor(dipTokenActor)) {
          let _ = switch (await dipTokenActor.approve(sonicCanisterId, sellAmounts[i])) {
            case (#Ok(id)) { #Ok(id) };
            case (#Err(e)) { #Err("token transfer failed:") };
          };
        };
        case (#ICRC1TokenActor(icrc1TokenActor)) {
          // transfer token from canister to sonic
          let getSubbaccount : Blob = await sonicCanister.initiateICRC1Transfer();
          let transferArgs : ICRCTransferArg = {
            from_subaccount = null;
            to : ICRCAccount = {
              owner = sonicCanisterId;
              subaccount = ?getSubbaccount;
            };
            amount = transferAmount;
          };
          let _ = await icrc1TokenActor.icrc1_transfer(transferArgs);
        };
        case (#ICRC2TokenActor(icrc2TokenActor)) {
          let _ = switch (await icrc2TokenActor.icrc2_approve(null, sonicCanisterId, sellAmounts[i])) {
            case (#Ok(id)) { #Ok(id) };
            case (#Err(e)) { #Err("token transfer failed:") };
          };
        };
      };
      let _ = await sonicCanister.deposit(sellingTokens[i], sellAmounts[i]);
      let _ = await swapTokensWithSonic(Principal.toText(sellingTokens[i]), Principal.toText(midToken), Principal.fromActor(Appic_Multiswap), sellAmounts[i]);
      let sonicBalanceOfBuyTokenAfterTrade = await sonicCanister.balanceOf(Principal.toText(midToken), Principal.fromActor(Appic_Multiswap));
      let amountOfBoughtToken = sonicBalanceOfBuyTokenAfterTrade - sonicBalanceOfBuyTokenBeforeTrade;
      let withdrawResult = await _WithdrawFromSonic(midToken, amountOfBoughtToken);
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
      let sonicBalanceOfBuyTokenBeforeTradeN = await sonicCanister.balanceOf(Principal.toText(midToken), Principal.fromActor(Appic_Multiswap));
      let buyActulAmt = buyAmounts[i] * midTokenBal / 100;
      switch (tokenActor) {
        case (#DIPtokenActor(dipTokenActor)) {
          let _ = switch (await dipTokenActor.approve(sonicCanisterId, buyActulAmt)) {
            case (#Ok(id)) { #Ok(id) };
            case (#Err(e)) { #Err("token transfer failed:") };
          };
        };
        case (#ICRC1TokenActor(icrc1TokenActor)) {
          let getSubbaccount : Blob = await sonicCanister.initiateICRC1Transfer();
          let transferArgs : ICRCTransferArg = {
            from_subaccount = null;
            to : ICRCAccount = {
              owner = sonicCanisterId;
              subaccount = ?getSubbaccount;
            };
            amount = transferAmount;
          };
          let _ = await icrc1TokenActor.icrc1_transfer(transferArgs);
        };
        case (#ICRC2TokenActor(icrc2TokenActor)) {
          let _ = switch (await icrc2TokenActor.icrc2_approve(null, sonicCanisterId, buyActulAmt)) {
            case (#Ok(id)) { #Ok(id) };
            case (#Err(e)) { #Err("token transfer failed:") };
          };
        };
      };
      let _ = await sonicCanister.deposit(midToken, buyActulAmt);
      let _ = await swapTokensWithSonic(Principal.toText(Principal.toText(midToken), buyingTokens[i]), Principal.fromActor(Appic_Multiswap), buyActulAmt);
      let sonicBalanceOfBuyTokenAfterTradeN = await sonicCanister.balanceOf(Principal.toText(buyingTokens[i]), Principal.fromActor(Appic_Multiswap));
      let amountOfBoughtTokenN = sonicBalanceOfBuyTokenAfterTradeN - sonicBalanceOfBuyTokenBeforeTradeN;
      let withdrawResultN = await _WithdrawFromSonic(buyingTokens[i], amountOfBoughtToken);

      let _ = await withdrawTokens(buyingTokensType[i], caller, buyingTokens[i], amountOfBoughtTokenN);
    };

  };

  // Withdraw swapped funds from sonic
  private func _WithdrawFromSonic(
    buyToken,
    amountToWithdraw,
  ) : async TxReceipt {

    let withdrawResult : TxReceipt = await sonicCanister.withdraw(buyToken, amountToWithdraw);
    return withdrawResult;
  };

  /**
 * @notice Retrieves all user tokens and their amounts
 * @dev This function fetches all locked tokens for a specific user.
 * @param caller The principal of the user
 * @return A tuple containing an array of token IDs and their corresponding amounts
 */
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

  // transfer arg for transferring of tokens
  //  let transferArgs : ICRCTransferArg = {
  //       from_subaccount = null;
  //       to : ICRCAccount = {
  //         owner = AppIC_Multiswap canister id;
  //         subaccount = getSubbaccount();
  //       };
  public shared (msg) func getSubAccount() : Blob {
    let subAcc : Blob = getICRC1SubAccount(msg.caller);
    return subAcc;
  };

  private func getICRC1SubAccount(caller : Principal) : Blob {
    let subaccount = Utils.generateSubaccount({
      caller = caller;
      id = depositCounterV2;
    });
    return subaccount;
  };

};
