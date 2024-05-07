import TokenActor "mo:std/TokenActor";
import Principal "mo:std/Principal";
import Utils "mo:Utils";
import Text "mo:std/text";

actor TokenTransferCanister {

  // Types for handling different token standards
  type Nat = Nat64; // Adjust if needed based on your system's typical number range
  type TransferReceipt = variant { Ok : Nat; Err : Text };
  type TokenActorVariant = variant {
    DIPTokenActor : actor { transfer : (Principal, Nat) -> async Nat };
    YCTokenActor : actor { transfer : (Principal, Nat) -> async Nat };
    ICRC1TokenActor : actor { icrc1_transfer : (ICRCTransferArg) -> async Nat };
    ICRC2TokenActor : actor { icrc1_transfer : (ICRCTransferArg) -> async Nat };
  };

  // Definition for ICRC transfer argument
  type ICRCTransferArg = {
    from_subaccount : ?Blob;
    to : { owner : Principal; subaccount : ?Blob };
    amount : Nat;
  };

  // Transfers tokens based on the token standard
  public func transferTokens(tokenCanister : TokenActorVariant, caller : Principal, value : Nat) : async TransferReceipt {
    switch (tokenCanister) {
      case (#DIPTokenActor(dipTokenActor)) {
        return await performDIPTransfer(dipTokenActor, caller, value);
      };
      case (#YCTokenActor(ycTokenActor)) {
        return await performYCTransfer(ycTokenActor, caller, value);
      };
      case (#ICRC1TokenActor(icrc1TokenActor)) {
        return await performICRCTransfer(icrc1TokenActor, caller, value);
      };
      case (#ICRC2TokenActor(icrc2TokenActor)) {
        return await performICRCTransfer(icrc2TokenActor, caller, value);
      };
    };
  };

  // Helper functions for transfer operations
  private func performDIPTransfer(dipTokenActor : actor { transfer : (Principal, Nat) -> async Nat }, caller : Principal, value : Nat) : async TransferReceipt {
    let txId = await dipTokenActor.transfer(caller, value);
    return #Ok(txId);
  };

  private func performYCTransfer(ycTokenActor : actor { transfer : (Principal, Nat) -> async Nat }, caller : Principal, value : Nat) : async TransferReceipt {
    let txId = await ycTokenActor.transfer(caller, value);
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
