import Principal "mo:base/Principal";
import Text "mo:base/Text";
import HashMap "mo:base/HashMap";
import Nat "mo:base/Nat";
import Utils "./utils";
import Prelude "mo:base/Prelude";
import Buffer "mo:base/Buffer";

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

  private func _getTokenActorWithType(tokenId : Text, tokenType : Text) : TokenActorVariable {
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
    var tokenCanister : TokenActorVariable = _getTokenActorWithType(tokenId, tokenType);
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
    var tokenCanister : TokenActorVariable = _getTokenActorWithType(tokenId, tokenType);
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
   * Transfers tokens from the caller to a specified token canister.
   * @param tokenCanister The target token canister.
   * @param caller The caller initiating the transfer.
   * @param value The amount of tokens to transfer.
   * @param tokenID The ID of the token to transfer.
   */
  public func transferTokensToCanister(tokenId : Text, tokenType : Text, caller : Principal, value : Nat, tokenID : Principal) : async TransferReceipt {

    // Retrieve user token data or initialize if null
    let _ = switch (await _transferFrom(tokenId, tokenType, caller, value)) {
      case (#Ok(id)) { id };
      case (#Err(e)) { return #Err("token transfer failed:") };
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
    let newBalance = switch (userData.get(tokenID)) {
      case (null) { value };
      case (?current) { current + value };
    };
    switch (userData.get(tokenID)) {
      case (null) {
        userData.put(tokenID, newBalance);
      };
      case (?value) {
        let _ = userData.replace(tokenID, newBalance);
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
