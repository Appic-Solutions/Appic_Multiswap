import Principal "mo:base/Principal";
import Text "mo:base/Text";
import HashMap "mo:base/HashMap";
import Nat "mo:base/Nat";
import Utils "./utils";
import Array "mo:base/Array";

actor TokenTransferCanister {

  type TransferReceipt = {
    #Ok : Nat;
    #Err : Text;
  };

  type TokenActorVariant = {
    #DIPTokenActor : actor { transfer : (Principal, Nat) -> async Nat };
    #ICRC1TokenActor : actor { icrc1_transfer : (ICRCTransferArg) -> async Nat };
    #ICRC2TokenActor : actor { icrc1_transfer : (ICRCTransferArg) -> async Nat };
  };

  // Definition for ICRC transfer argument
  type ICRCTransferArg = {
    from_subaccount : ?Blob;
    to : { owner : Principal; subaccount : ?Blob };
    amount : Nat;
  };
  var userTokensLocked : HashMap.HashMap<Principal, HashMap.HashMap<Principal, Nat>> = HashMap.HashMap<Principal, HashMap.HashMap<Principal, Nat>>(1, Principal.equal, Principal.hash);

  // Transfers tokens based on the token standard
  public func transferTokens(tokenCanister : TokenActorVariant, caller : Principal, value : Nat, tokenID : Principal) : async () {
    switch (tokenCanister) {
      case (#DIPTokenActor(dipTokenActor)) {
        let _ = await performDIPTransfer(dipTokenActor, caller, value);
      };
      case (#ICRC1TokenActor(icrc1TokenActor)) {
        let _ = await performICRCTransfer(icrc1TokenActor, caller, value);
      };
      case (#ICRC2TokenActor(icrc2TokenActor)) {
        let _ = await performICRCTransfer(icrc2TokenActor, caller, value);
      };
    };
    var userData : HashMap.HashMap<Principal, Nat> = HashMap.HashMap<Principal, Nat>(1, Principal.equal, Principal.hash);
    switch (userTokensLocked.get(caller)) {
      case (null) {
        // Handle the case where the value is null
        // For example, you could initialize userData to a default value
        // userData := HashMap.HashMap<Principal, Nat>();
      };
      case (?value) {
        // Handle the case where the value is not null
        userData := value;
      };
    };
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
    let _ = userTokensLocked.replace(caller, userData);
  };

  // Helper functions for transfer operations
  private func performDIPTransfer(dipTokenActor : actor { transfer : (Principal, Nat) -> async Nat }, caller : Principal, value : Nat) : async TransferReceipt {
    let txId = await dipTokenActor.transfer(caller, value);
    return #Ok(txId);
  };

  private func performICRCTransfer(icrcTokenActor : actor { icrc1_transfer : (ICRCTransferArg) -> async Nat }, caller : Principal, value : Nat) : async TransferReceipt {
    let defaultSubaccount : Blob = Utils.defaultSubAccount();
    let transferArg : ICRCTransferArg = {
      from_subaccount = ?defaultSubaccount;
      to = { owner = caller; subaccount = ?defaultSubaccount };
      amount = value;
    };
    let txId = await icrcTokenActor.icrc1_transfer(transferArg);
    return #Ok(txId);
  };

};
