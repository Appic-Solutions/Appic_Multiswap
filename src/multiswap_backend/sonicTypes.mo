import Blob "mo:base/Blob";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
module {
    public type TxReceipt = Result.Result<Nat, Text>;
    public type sonicActor = actor {
        initiateICRC1Transfer : shared () -> async Blob;
        deposit : shared (Principal, Nat) -> async TxReceipt;
        swapExactTokensForTokens : shared (Nat, Nat, [Text], Principal, Int) -> async TxReceipt;
        withdraw : shared (Principal, Nat) -> async TxReceipt;
        balanceOf : shared query (Text, Principal) -> async Nat;
    };
    public func _getSonicActor(sonicCanisterId : Principal) : sonicActor {

        var sonicCanister : sonicActor = actor (Principal.toText(sonicCanisterId));
        return sonicCanister;

    };
};
