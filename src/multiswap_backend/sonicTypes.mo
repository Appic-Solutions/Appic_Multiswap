import Blob "mo:base/Blob";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
module {
    public type TxReceipt = Result.Result<Nat, Text>;
    public type PairInfoExt = {
        id : Text;
        token0 : Text; //Principal;
        token1 : Text; //Principal;
        creator : Principal;
        reserve0 : Nat;
        reserve1 : Nat;
        price0CumulativeLast : Nat;
        price1CumulativeLast : Nat;
        kLast : Nat;
        blockTimestampLast : Int;
        totalSupply : Nat;
        lptoken : Text;
    };
    public type sonicActor = actor {
        initiateICRC1Transfer : shared () -> async Blob;
        deposit : shared (Principal, Nat) -> async TxReceipt;
        swapExactTokensForTokens : shared (Nat, Nat, [Text], Principal, Int) -> async TxReceipt;
        withdraw : shared (Principal, Nat) -> async TxReceipt;
        balanceOf : shared query (Text, Principal) -> async Nat;
        getPair(token0 : Principal, token1 : Principal) : async ?PairInfoExt;
    };
    public func _getSonicActor(sonicCanisterId : Principal) : sonicActor {
        var sonicCanister : sonicActor = actor (Principal.toText(sonicCanisterId));
        return sonicCanister;

    };
};
