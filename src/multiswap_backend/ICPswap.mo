import Principal "mo:base/Principal";

type Token = {
    address : Principal;
    standard : Text;
};

type GetPoolArgs = {
    fee : Nat;
    token0 : Token;
    token1 : Token;
};

type PoolData = {
    fee : Nat;
    key : Text;
    tickSpacing : Int;
    token0 : Token;
    token1 : Token;
    canisterId : Principal;
};

type Error = {
    #CommonError;
    #InternalError : Text;
    #UnsupportedToken : Text;
    #InsufficientFunds;
};

type Result_2 = {
    #ok : [PoolData];
    #err : Error;
};

type Result_3 = {
    #ok : PoolData;
    #err : Error;
};

type SwapFactory = actor {
    getPool : (GetPoolArgs) -> async Result_3;
    getPools : () -> async Result_2;
};

type SwapArgs = {
    amountIn : Text;
    zeroForOne : Bool;
    amountOutMinimum : Text;
};

type QuoteResult = {
    #ok : Nat;
    #err : Error;
};

type SwapPool = actor {
    quote : (SwapArgs) -> async QuoteResult;
};

actor TokenSwapCanister {

    let swapFactoryCanister = actor ("4mmnk-kiaaa-aaaag-qbllq-cai") : SwapFactory;

    public func getPrice(token0Address : Principal, token0Standard : Text, token1Address : Principal, token1Standard : Text) : async QuoteResult {
        let token0 = { address = token0Address; standard = token0Standard };
        let token1 = { address = token1Address; standard = token1Standard };

        let poolArgs = { fee = 3000; token0; token1 };

        switch (await swapFactoryCanister.getPool(poolArgs)) {
            case (#ok(poolData)) {
                let swapPoolCanister : SwapPool = actor (Principal.toText(poolData.canisterId));

                // Example swap arguments
                let swapArgs = {
                    amountIn = "100000000"; // Example input amount, 1 ICP with 8 decimals
                    zeroForOne = true; // Assuming token0 is ICP and token1 is ckBTC
                    amountOutMinimum = "0"; // No minimum amount restriction
                };

                switch (await swapPoolCanister.quote(swapArgs)) {
                    case (#ok(amountOut)) {
                        return #ok(amountOut);
                    };
                    case (#err(error)) {
                        return #err(error);
                    };
                };
            };
            case (#err(error)) {
                return #err((error));
            };
        };
    };

};
