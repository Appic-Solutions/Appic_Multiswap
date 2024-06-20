import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Utils "./utils";
import Prelude "mo:base/Prelude";
import sonicTypes "sonicTypes";
import Time "mo:base/Time";
import Iter "mo:base/Iter";
import Blob "mo:base/Blob";
import Array "mo:base/Array";
import Nat8 "mo:base/Nat8";
import Bool "mo:base/Bool";
import Account "Account";

actor Appic_Multiswap {
  type Account = {
    owner : Principal;
    subaccount : ?Subaccount;
  };
  type ApproveArg = {
    spender : Account;
    amount : Nat;
  };
  type ERR = {
    #BadFee : { expected_fee : Nat };
    #InsufficientFunds : { balance : Nat };
    #AllowanceChanged : { current_allowance : Nat };
    #Expired : { ledger_time : Nat64 };
    #TooOld;
    #CreatedInFuture : { ledger_time : Nat64 };
    #Duplicate : { duplicate_of : Nat };
    #TemporarilyUnavailable;
    #GenericError : { error_code : Nat; message : Text };
    #BadBurn : { min_burn_amount : Nat };
    #InsufficientAllowance : { allowance : Nat };
  };
  type TransferReceipt = {
    #Ok : Nat; // Indicates a successful transfer with a transaction ID
    #Err : ERR; // Indicates an error occurred during the transfer
  };
  type sonicActor = sonicTypes.sonicActor;
  type TxReceipt = sonicTypes.TxReceipt;
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
  type TokenActorVariable = {
    #DIPtokenActor : TokenActor;
    #ICRC1TokenActor : ICRC1TokenActor;
    #ICRC2TokenActor : ICRC2TokenActor;
  };
  type Token = {
    address : Text;
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
  type Result_3 = {
    #ok : PoolData;
    #err : Error;
  };
  type SwapFactory = actor {
    getPool : (GetPoolArgs) -> async Result_3;
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
  type DepositArgs = { fee : Nat; token : Text; amount : Nat };
  type Result = {
    #ok : Nat;
    #err : Error;
  };
  type WithdrawArgs = { fee : Nat; token : Text; amount : Nat };

  type SwapPool = actor {
    quote : (SwapArgs) -> async QuoteResult;
    deposit : (DepositArgs) -> async Result;
    depositFrom : (DepositArgs) -> async Result;
    swap : (SwapArgs) -> async Result;
    withdraw : (WithdrawArgs) -> async Result;
  };

  public type TokenActor = actor {
    allowance : shared (owner : Principal, spender : Principal) -> async Nat;
    approve : shared (spender : Principal, value : Nat) -> async TransferReceipt;
    balanceOf : (owner : Principal) -> async Nat;
    decimals : () -> async Nat8;
    name : () -> async Text;
    symbol : () -> async Text;
    totalSupply : () -> async Nat;
    getTokenFee : () -> async Nat;
    transfer : shared (to : Principal, value : Nat) -> async TransferReceipt;
    transferFrom : shared (from : Principal, to : Principal, value : Nat) -> async TransferReceipt;
  };

  public type ICRC1TokenActor = actor {
    icrc1_balance_of : (account : ICRCAccount) -> async Nat;
    icrc1_decimals : () -> async Nat8;
    icrc1_name : () -> async Text;
    icrc1_symbol : () -> async Text;
    icrc1_total_supply : () -> async Nat;
    icrc1_fee : () -> async Nat;
    icrc1_transfer : shared (ICRCTransferArg) -> async TransferReceipt;
  };
  public type ICRC2TokenActor = actor {
    icrc2_approve : shared (ApproveArg) -> async TransferReceipt;
    icrc2_allowance : shared (account : Subaccount, spender : Principal) -> async (allowance : Nat, expires_at : ?Nat64);
    icrc1_balance_of : (account : ICRCAccount) -> async Nat;
    icrc1_decimals : () -> async Nat8;
    icrc1_name : () -> async Text;
    icrc1_symbol : () -> async Text;
    icrc1_total_supply : () -> async Nat;
    icrc1_fee : () -> async Nat;
    icrc2_transfer_from : shared (ICRC2TransferArg) -> async TransferReceipt;
    icrc1_transfer : shared (ICRCTransferArg) -> async TransferReceipt;
  };

  let sonicCanisterId : Principal = Principal.fromText("3xwpq-ziaaa-aaaah-qcn4a-cai");
  let sonicCanister : sonicActor = sonicTypes._getSonicActor(sonicCanisterId); // Sonic canister
  let swapFactoryCanister = actor ("4mmnk-kiaaa-aaaag-qbllq-cai") : SwapFactory;
  var multiswap_fee : Nat = 0;
  var owner : Principal = Principal.fromText("ylzdl-4ynxq-btau6-p3vdx-vigzg-s5c3s-7lidk-ivg4i-pqoe2-plgro-4ae");

  /**
   * @notice Get the token actor with its type
   * @dev This function returns the token actor variable based on the token type
   * @param tokenId The ID of the token
   * @param tokenType The type of the token (e.g., "DIP20", "ICRC1", "ICRC2")
   * @return The token actor variable
   */
  public query func _getTokenActorWithType(tokenId : Text, tokenType : Text) : async TokenActorVariable {
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
    let tokenCanister : TokenActorVariable = await _getTokenActorWithType(tokenId, tokenType);
    let fee = await getfeeToken(tokenId, tokenType);
    switch (tokenCanister) {
      case (#DIPtokenActor(dipTokenActor)) {
        let txid : TransferReceipt = await dipTokenActor.transferFrom(caller, Principal.fromActor(Appic_Multiswap), value -fee);
        return txid;
      };
      case (#ICRC1TokenActor(icrc1TokenActor)) {
        let defaultSubaccount : Subaccount = Utils.defaultSubAccount();
        let userSubAccount : Subaccount = await getICRC1SubAccount(caller);
        let transferArg : ICRCTransferArg = {
          from_subaccount = ?userSubAccount;
          to = {
            owner = Principal.fromActor(Appic_Multiswap);
            subaccount = ?defaultSubaccount;
          };
          amount = value -fee;
        };
        let txid : TransferReceipt = await icrc1TokenActor.icrc1_transfer(transferArg);
        return txid;
      };
      case (#ICRC2TokenActor(icrc2TokenActor)) {
        let defaultSubaccount : Subaccount = Utils.defaultSubAccount();
        let transferArg : ICRC2TransferArg = {
          from = { owner = caller; subaccount = ?defaultSubaccount };
          to = {
            owner = Principal.fromActor(Appic_Multiswap);
            subaccount = ?defaultSubaccount;
          };
          amount = value -fee;
        };
        let txid : TransferReceipt = await icrc2TokenActor.icrc2_transfer_from(transferArg);
        return txid;
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
    let tokenCanister : TokenActorVariable = await _getTokenActorWithType(tokenId, tokenType);
    let fee = await getfeeToken(tokenId, tokenType);
    switch (tokenCanister) {
      case (#DIPtokenActor(dipTokenActor)) {
        let txid : TransferReceipt = await dipTokenActor.transfer(caller, (value -fee));
        return txid;
      };
      case (#ICRC1TokenActor(icrc1TokenActor)) {
        let defaultSubaccount : Subaccount = Utils.defaultSubAccount();
        let transferArg : ICRCTransferArg = {
          from_subaccount = ?defaultSubaccount;
          to = { owner = caller; subaccount = ?defaultSubaccount };
          amount = value -fee;
        };
        let txid : TransferReceipt = await icrc1TokenActor.icrc1_transfer(transferArg);
        return txid;
      };
      case (#ICRC2TokenActor(icrc2TokenActor)) {
        let defaultSubaccount : Blob = Utils.defaultSubAccount();
        let transferArg : ICRCTransferArg = {
          from_subaccount = ?defaultSubaccount;
          to = { owner = caller; subaccount = ?defaultSubaccount };
          amount = value -fee;
        };
        let txid : TransferReceipt = await icrc2TokenActor.icrc1_transfer(transferArg);
        return txid;
      };

    };
  };

  /// @notice Swaps tokens using the SonicSwap platform
  /// @param sellToken The principal of the token to sell
  /// @param buyToken The principal of the token to buy
  /// @param sellTokenType The standard of the token to sell
  /// @param sellAmt The amount of the token to sell
  /// @return The amount of the bought token received
  public func swapWithSonic(sellToken : Principal, buyToken : Principal, sellTokenType : Text, buyTokenType : Text, sellAmt : Nat) : async Nat {
    // Get the token actor for the sell token
    let tokenActor : TokenActorVariable = await _getTokenActorWithType(Principal.toText(sellToken), sellTokenType);

    // Get the balance of the buy token before the trade
    let sonicBalanceOfBuyTokenBeforeTrade = await sonicCanister.balanceOf(Principal.toText(buyToken), Principal.fromActor(Appic_Multiswap));
    var fee = 0;
    // Approve or transfer tokens to SonicSwap based on the token type
    switch (tokenActor) {
      case (#DIPtokenActor(dipTokenActor)) {
        // Approve SonicSwap to transfer tokens on behalf of the user
        let _ = await dipTokenActor.approve(sonicCanisterId, sellAmt);
        fee := await dipTokenActor.getTokenFee();
      };
      case (#ICRC1TokenActor(icrc1TokenActor)) {
        // Transfer ICRC1 tokens to SonicSwap
        let getSubbaccount : Blob = await sonicCanister.initiateICRC1Transfer();
        fee := await icrc1TokenActor.icrc1_fee();
        let transferArgs : ICRCTransferArg = {
          from_subaccount = null;
          to : ICRCAccount = {
            owner = sonicCanisterId;
            subaccount = ?getSubbaccount;
          };
          amount = sellAmt -fee;
        };
        let _ = await icrc1TokenActor.icrc1_transfer(transferArgs);
      };
      case (#ICRC2TokenActor(icrc2TokenActor)) {
        // Approve SonicSwap to transfer tokens on behalf of the user
        let arg : ApproveArg = {
          spender = {
            owner = sonicCanisterId;
            subaccount = null;
          };
          amount = sellAmt;
        };
        let _ = await icrc2TokenActor.icrc2_approve(arg);
        fee := await icrc2TokenActor.icrc1_fee();
      };
    };

    // Deposit the sell tokens to SonicSwap
    let _ = await sonicCanister.deposit(sellToken, sellAmt -(2 * fee));

    let sonicBalanceOfSellToken = await sonicCanister.balanceOf(Principal.toText(sellToken), Principal.fromActor(Appic_Multiswap));
    // Perform the token swap with SonicSwap
    let _ = await sonicCanister.swapExactTokensForTokens(sonicBalanceOfSellToken, 0, [Principal.toText(sellToken), Principal.toText(buyToken)], Principal.fromActor(Appic_Multiswap), Time.now() + 3000000000000);

    let sonicBalanceOfBuyTokenAfterTrade = await sonicCanister.balanceOf(Principal.toText(buyToken), Principal.fromActor(Appic_Multiswap));

    // Calculate the amount of bought token received
    let amountOfBoughtToken = sonicBalanceOfBuyTokenAfterTrade - sonicBalanceOfBuyTokenBeforeTrade;

    let fee_buy = await getfeeToken(Principal.toText(buyToken), buyTokenType);
    // Withdraw the bought tokens from SonicSwap
    let _ = switch (await sonicCanister.withdraw(buyToken, amountOfBoughtToken -fee_buy)) {
      case (#ok(_)) {};
      case (#err(_)) {
        assert (false);
      };
    };
    let tokenActor1 : TokenActorVariable = await _getTokenActorWithType(Principal.toText(buyToken), buyTokenType);
    switch (tokenActor1) {
      case (#DIPtokenActor(dipTokenActor)) {
        let tokenget = await dipTokenActor.balanceOf(Principal.fromActor(Appic_Multiswap));
        return tokenget;
      };
      case (#ICRC1TokenActor(icrc1TokenActor)) {
        let tokenget = await icrc1TokenActor.icrc1_balance_of({
          owner = Principal.fromActor(Appic_Multiswap);
          subaccount = null;
        });
        return tokenget;
      };
      case (#ICRC2TokenActor(icrc2TokenActor)) {
        let tokenget = await icrc2TokenActor.icrc1_balance_of({
          owner = Principal.fromActor(Appic_Multiswap);
          subaccount = null;
        });
        return tokenget;
      };
    };
  };

  /// @notice Swaps tokens using the ICPSwap platform
  /// @param sellToken The principal of the token to sell
  /// @param buyToken The principal of the token to buy
  /// @param sellTokenType The standard of the token to sell
  /// @param buyTokenType The standard of the token to buy
  /// @param sellAmt The amount of the token to sell
  /// @return The amount of the bought token received
  public func swapWithICPSwap(
    sellToken : Text,
    buyToken : Text,
    sellTokenType : Text,
    buyTokenType : Text,
    sellAmt : Nat,
  ) : async Nat {
    let token0 = { address = sellToken; standard = sellTokenType };
    let token1 = { address = buyToken; standard = buyTokenType };
    let poolArgs = { fee = 3000; token0; token1 };
    switch (await swapFactoryCanister.getPool(poolArgs)) {
      case (#ok(poolData)) {
        let swapPoolCanister : SwapPool = actor (Principal.toText(poolData.canisterId));
        let swapPoolCanisterId : Principal = poolData.canisterId;

        let tokenActor : TokenActorVariable = await _getTokenActorWithType(sellToken, sellTokenType);
        var fee_sell = 0;

        switch (tokenActor) {
          case (#DIPtokenActor(dipTokenActor)) {
            // Approve ICPSwap to transfer tokens on behalf of the user
            let _ = await dipTokenActor.approve(swapPoolCanisterId, sellAmt);
            fee_sell := await dipTokenActor.getTokenFee();
            // Deposit tokens to the SwapPool
            let depositArgs : DepositArgs = {
              fee = fee_sell;
              token = sellToken;
              amount = sellAmt -fee_sell;
            };
            let _ = await swapPoolCanister.depositFrom(depositArgs);
          };
          case (#ICRC1TokenActor(icrc1TokenActor)) {
            // Transfer ICRC1 tokens to SonicSwap
            let getSubbaccount : Blob = await principalToBlobICPswap(Principal.fromActor(Appic_Multiswap));
            fee_sell := await icrc1TokenActor.icrc1_fee();
            let transferArgs : ICRCTransferArg = {
              from_subaccount = null;
              to : ICRCAccount = {
                owner = swapPoolCanisterId;
                subaccount = ?getSubbaccount;
              };
              amount = sellAmt -fee_sell;
            };
            let _ = await icrc1TokenActor.icrc1_transfer(transferArgs);
            // Deposit tokens to the SwapPool
            let depositArgs : DepositArgs = {
              fee = fee_sell;
              token = sellToken;
              amount = sellAmt -fee_sell;
            };
            let _ = await swapPoolCanister.deposit(depositArgs);
          };
          case (#ICRC2TokenActor(icrc2TokenActor)) {
            // Approve ICPSwap to transfer tokens on behalf of the user
            let getSubbaccount : Blob = await principalToBlobICPswap(Principal.fromActor(Appic_Multiswap));
            fee_sell := await icrc2TokenActor.icrc1_fee();
            let transferArgs : ICRCTransferArg = {
              from_subaccount = null;
              to : ICRCAccount = {
                owner = swapPoolCanisterId;
                subaccount = ?getSubbaccount;
              };
              amount = sellAmt -fee_sell;
            };
            let _ = await icrc2TokenActor.icrc1_transfer(transferArgs);
            let depositArgs : DepositArgs = {
              fee = fee_sell;
              token = sellToken;
              amount = sellAmt -fee_sell;
            };
            let _ = await swapPoolCanister.deposit(depositArgs);
          };
        };
        let zto : Bool = poolData.token0.address == sellToken;
        // Perform the swap
        let swapArgs : SwapArgs = {
          amountIn = Nat.toText(sellAmt -fee_sell);
          zeroForOne = zto; // Adjust as necessary based on trade direction
          amountOutMinimum = "0"; // No minimum amount restriction
        };
        let amountOut = switch (await swapPoolCanister.swap(swapArgs)) {
          case (#ok(amountOut)) { amountOut };
          case (#err(_)) {
            assert (false);
            0;
          };
        };
        let tokenActor1 : TokenActorVariable = await _getTokenActorWithType(buyToken, buyTokenType);
        var fee_buy = 0;
        switch (tokenActor1) {
          case (#DIPtokenActor(dipTokenActor)) {
            fee_buy := await dipTokenActor.getTokenFee();
          };
          case (#ICRC1TokenActor(icrc1TokenActor)) {
            fee_buy := await icrc1TokenActor.icrc1_fee();
          };
          case (#ICRC2TokenActor(icrc2TokenActor)) {
            fee_buy := await icrc2TokenActor.icrc1_fee();
          };
        };
        // Withdraw the bought tokens from SwapPool
        let withdrawArgs = {
          fee = fee_buy;
          token = buyToken;
          amount = amountOut -fee_buy;
        };
        let _ = switch (await swapPoolCanister.withdraw(withdrawArgs)) {
          case (#ok(id)) { #ok(id) };
          case (#err(e)) {
            assert (false);
            #err(e);
          };
        };
        switch (tokenActor1) {
          case (#DIPtokenActor(dipTokenActor)) {
            let tokenget = await dipTokenActor.balanceOf(Principal.fromActor(Appic_Multiswap));
            return tokenget;
          };
          case (#ICRC1TokenActor(icrc1TokenActor)) {
            let tokenget = await icrc1TokenActor.icrc1_balance_of({
              owner = Principal.fromActor(Appic_Multiswap);
              subaccount = null;
            });
            return tokenget;
          };
          case (#ICRC2TokenActor(icrc2TokenActor)) {
            let tokenget = await icrc2TokenActor.icrc1_balance_of({
              owner = Principal.fromActor(Appic_Multiswap);
              subaccount = null;
            });
            return tokenget;
          };
        };
      };
      case (#err(e)) {
        assert (false);
        return 0;
      };
    };
  };

  /// @notice Converts a Principal to a 32-byte Blob for ICPswap
  /// @param p The principal to convert
  /// @return The 32-byte Blob representation of the principal
  private func principalToBlobICPswap(p : Principal) : async Blob {
    // Convert the principal to a byte array
    var arr : [Nat8] = Blob.toArray(Principal.toBlob(p));

    // Initialize a 32-byte array filled with zeros
    var defaultArr : [var Nat8] = Array.init<Nat8>(32, 0);

    // Set the first byte to the size of the principal's byte array
    defaultArr[0] := Nat8.fromNat(arr.size());

    // Copy the principal's byte array into the initialized array
    var ind : Nat = 0;
    while (ind < arr.size() and ind < 32) {
      defaultArr[ind + 1] := arr[ind];
      ind := ind + 1;
    };

    // Return the 32-byte Blob
    return Blob.fromArray(Array.freeze(defaultArr));
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
  public shared (msg) func multiswap(
    sellingTokens : [Principal], // List of tokens being sold
    buyingTokens : [Principal], // List of tokens being bought
    sellAmounts : [Nat], // Amounts of tokens being sold
    buyAmounts : [Nat], // Amounts of tokens being bought
    midToken : Principal, // The middle token used for swaps
    midTokenType : Text, // Type of the middle token
    sellingTokensType : [Text], // Types of the selling tokens
    buyingTokensType : [Text], // Types of the buying tokens
  ) : async () {
    let caller : Principal = msg.caller;
    // Ensure the number of selling tokens matches the number of sell amounts
    assert (sellingTokens.size() == sellAmounts.size());
    // Ensure the number of buying tokens matches the number of buy amounts
    assert (buyingTokens.size() == buyAmounts.size());

    // Retrieve the actor for the mid token based on its type
    let midTokenActor : TokenActorVariable = await _getTokenActorWithType(Principal.toText(midToken), midTokenType);
    var midTokenBalInit : Nat = 0;

    // Get the initial balance of the middle token in the canister
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
    // Loop through each selling token to perform swaps
    for (i in Iter.range(0, sellingTokens.size() - 1)) {
      // Transfer the selling tokens to the canister
      let fee = await getfeeToken(Principal.toText(sellingTokens[i]), sellingTokensType[i]);

      let _ = await _transferFrom(Principal.toText(sellingTokens[i]), sellingTokensType[i], caller, sellAmounts[i]);

      // Calculate the amount out using SonicSwap
      let sonicAmountOut : Nat = await sonicSwapAmountOut(sellingTokens[i], midToken, sellAmounts[i] -fee);
      // Calculate the amount out using ICPSwap
      let icpAmountOut : Nat = await icpSwapAmountOut(Principal.toText(sellingTokens[i]), sellingTokensType[i], Principal.toText(midToken), midTokenType, sellAmounts[i] -fee);
      if (sellingTokens[i] != midToken) {
        // Compare the results and choose the better option for swapping
        if (sonicAmountOut > icpAmountOut) {
          let _ = await swapWithSonic(sellingTokens[i], midToken, sellingTokensType[i], midTokenType, sellAmounts[i] -fee);
        } else if (sonicAmountOut < icpAmountOut) {
          let _ = await swapWithICPSwap(Principal.toText(sellingTokens[i]), Principal.toText(midToken), sellingTokensType[i], midTokenType, sellAmounts[i] -fee);
        } else {
          assert (false);
        };
      };
    };

    // // Get the final balance of the middle token in the canister after swaps
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
    // // Calculate the middle token balance gained through swaps and apply a fee
    var midTokenBal = midTokenBalFin - midTokenBalInit;
    midTokenBal := (1000 - multiswap_fee) * midTokenBal / 1000;
    let feeTrans = midTokenBalFin - (midTokenBalInit + midTokenBal);

    // Withdraw the fee amount to the owner's account
    let _ = switch (await _transfer(Principal.toText(midToken), midTokenType, owner, feeTrans)) {
      case (#Ok(id)) { #Ok(id) };
      case (#Err(e)) {
        assert (false);
        #Err(e);
      };
    };

    // Loop through each buying token to perform swaps
    for (i in Iter.range(0, buyingTokens.size() - 1)) {
      // Calculate the actual amount to buy based on the middle token balance
      let buyActulAmt = buyAmounts[i] * midTokenBal / 100;

      // Calculate the amount out using SonicSwap
      let sonicAmountOut : Nat = await sonicSwapAmountOut(midToken, buyingTokens[i], buyActulAmt);
      // Calculate the amount out using ICPSwap
      let icpAmountOut : Nat = await icpSwapAmountOut(Principal.toText(midToken), midTokenType, Principal.toText(buyingTokens[i]), buyingTokensType[i], buyActulAmt);

      // Compare the results and choose the better option for swapping
      if (buyingTokens[i] != midToken) {
        if (sonicAmountOut > icpAmountOut) {
          let amountOfBoughtTokenN = await swapWithSonic(midToken, buyingTokens[i], midTokenType, buyingTokensType[i], buyActulAmt);
          // Withdraw the bought tokens to the caller's account
          let _ = switch (await _transfer(Principal.toText(buyingTokens[i]), buyingTokensType[i], caller, amountOfBoughtTokenN)) {
            case (#Ok(id)) { #Ok(id) };
            case (#Err(e)) {
              assert (false);
              #Err(e);
            };
          };
        } else if (sonicAmountOut < icpAmountOut) {
          let amountOfBoughtTokenN = await swapWithICPSwap(Principal.toText(midToken), Principal.toText(buyingTokens[i]), midTokenType, buyingTokensType[i], buyActulAmt);
          // Withdraw the bought tokens to the caller's account
          let _ = switch (await _transfer(Principal.toText(buyingTokens[i]), buyingTokensType[i], caller, amountOfBoughtTokenN)) {
            case (#Ok(id)) { #Ok(id) };
            case (#Err(e)) {
              assert (false);
              #Err(e);
            };
          };
        } else {
          assert (false);
        };
      } else {
        let _ = switch (await _transfer(Principal.toText(buyingTokens[i]), buyingTokensType[i], caller, buyActulAmt)) {
          case (#Ok(id)) { #Ok(id) };
          case (#Err(e)) {
            assert (false);
            #Err(e);
          };
        };
      };
    };
  };

  // transfer arg for transferring of ICRC1 tokens
  //  let transferArgs : ICRCTransferArg = {
  //       from_subaccount = null;
  //       to : ICRCAccount = {
  //         owner = AppIC_Multiswap canister id;
  //         subaccount = getSubbaccount();
  //       };

  /// @notice Generates a subaccount for ICRC1 tokens for a given caller
  /// @param caller The principal of the caller
  /// @return subaccount The generated subaccount as a Blob
  public func getICRC1SubAccount(caller : Principal) : async Subaccount {
    // Generate the subaccount with the specified caller and a fixed ID of 1000
    let subaccount : Subaccount = Utils.generateSubaccount({
      caller = caller;
      id = 1000;
    });
    // Return the generated subaccount
    return subaccount;
  };

  /// @notice Changes the fee for the multiswap
  /// @param val The new fee value to be set
  /// @notice Can only be called by the owner
  public shared (msg) func changeFee(val : Nat) : async () {
    // Ensure the caller is the owner
    assert (msg.caller == owner);
    // Set the new fee value
    multiswap_fee := val;
  };

  /// @notice Changes the owner of the multiswap
  /// @param newOwner The principal of the new owner
  /// @notice Can only be called by the current owner
  public shared (msg) func changeOwner(newOwner : Principal) : async () {
    // Ensure the caller is the owner
    assert (msg.caller == owner);
    // Set the new owner
    owner := newOwner;
  };

  /// @notice Withdraws ICRC1 tokens from the canister to the specified account
  /// @param tokenPrincipal The principal of the token canister
  public shared (msg) func withdrawTransferICRC1(tokenPrincipal : Principal) : async () {
    // Retrieve the token actor for the given principal and type ICRC1
    var tokenCanister : TokenActorVariable = await _getTokenActorWithType(Principal.toText(tokenPrincipal), "ICRC1");
    switch (tokenCanister) {
      case (#DIPtokenActor(dipTokenActor)) {
        // Do nothing for DIP tokens
        return;
      };
      case (#ICRC1TokenActor(icrc1TokenActor)) {
        // Get the subaccount for the current user
        var userSubAccount : Subaccount = await getICRC1SubAccount(msg.caller);
        // Define the account with the principal of the canister and the user's subaccount
        let account : ICRCAccount = {
          owner = Principal.fromActor(Appic_Multiswap);
          subaccount = ?userSubAccount;
        };
        // Get the balance of the ICRC1 tokens in the specified account
        let bal = await icrc1TokenActor.icrc1_balance_of(account);
        let fee = await icrc1TokenActor.icrc1_fee();
        if (bal < fee) {
          // If balance is zero, do nothing
          return;
        } else {
          // Otherwise, create a default subaccount for the transfer
          let getSubbaccount : Blob = Utils.defaultSubAccount();
          // Prepare the transfer arguments
          let transferArgs : ICRCTransferArg = {
            from_subaccount = null;
            to : ICRCAccount = {
              owner = msg.caller;
              subaccount = ?getSubbaccount;
            };
            amount = bal -fee;
          };
          // Perform the transfer and handle the result
          let _ = switch (await icrc1TokenActor.icrc1_transfer(transferArgs)) {
            case (#Ok(id)) { #Ok(id) };
            case (#Err(e)) {
              #Err(e);
            };
          };
        };
      };
      case (#ICRC2TokenActor(icrc2TokenActor)) {
        // Do nothing for ICRC2 tokens
        return;
      };
    };
  };

  /// @notice Calculates the amount of token1 received for a given amount of token0 using SonicSwap
  /// @param t0 The principal of the first token
  /// @param t1 The principal of the second token
  /// @param amountIn The amount of the first token to swap
  /// @return The amount of the second token received
  private func sonicSwapAmountOut(t0 : Principal, t1 : Principal, amountIn : Nat) : async Nat {
    // Retrieve the reserves for the token pair from SonicSwap
    let ret : (Nat, Nat) = switch (await sonicCanister.getPair(t0, t1)) {
      case (?p) {
        if (p.token0 == Principal.toText(t0)) {
          (p.reserve0, p.reserve1);
        } else {
          (p.reserve1, p.reserve0);
        };
      };
      case (_) {
        // If no pair exists, return 0
        return 0;
      };
    };
    // Extract the reserves for token0 and token1
    let reserveIn = ret.0;
    let reserveOut = ret.1;
    // Calculate the output amount using utility function
    let data = Utils.getAmountOut(amountIn, reserveIn, reserveOut);
    return data.0;
  };

  /// @notice Calculates the amount of token1 received for a given amount of token0 using ICPSwap
  /// @param token0Address The principal of the first token
  /// @param token0Standard The standard of the first token
  /// @param token1Address The principal of the second token
  /// @param token1Standard The standard of the second token
  /// @param amountIn The amount of the first token to swap
  /// @return The amount of the second token received
  private func icpSwapAmountOut(
    token0Address : Text,
    token0Standard : Text,
    token1Address : Text,
    token1Standard : Text,
    amountIn : Nat,
  ) : async Nat {
    // Define the token pair details
    let token0 = { address = token0Address; standard = token0Standard };
    let token1 = { address = token1Address; standard = token1Standard };

    // Prepare the pool arguments
    let poolArgs : GetPoolArgs = { fee = 3000; token0; token1 };

    // Retrieve the swap pool from the swap factory canister
    switch (await swapFactoryCanister.getPool(poolArgs)) {
      case (#ok(poolData)) {
        // Define the swap pool canister
        let swapPoolCanister : SwapPool = actor (Principal.toText(poolData.canisterId));
        let zto : Bool = poolData.token0.address == token0Address;

        // Prepare the swap arguments
        let swapArgs = {
          amountIn = Nat.toText(amountIn);
          zeroForOne = zto;
          amountOutMinimum = "0"; // No minimum amount restriction
        };

        // Perform the quote operation and handle the result
        switch (await swapPoolCanister.quote(swapArgs)) {
          case (#ok(amountOut)) {
            return (amountOut);
          };
          case (#err(error)) {
            return 0;
          };
        };
      };
      case (#err(error)) {
        return 0;
      };
    };
  };

  /// @notice Calculte the fee of token
  /// @param token0Address The principal of the  token
  /// @param token0Standard The standard of the  token
  /// @return The amount of the fee
  public func getfeeToken(
    token0Address : Text,
    token0Standard : Text,
  ) : async Nat {
    let tokenActor1 : TokenActorVariable = await _getTokenActorWithType(token0Address, token0Standard);
    switch (tokenActor1) {
      case (#DIPtokenActor(dipTokenActor)) {
        await dipTokenActor.getTokenFee();
      };
      case (#ICRC1TokenActor(icrc1TokenActor)) {
        await icrc1TokenActor.icrc1_fee();
      };
      case (#ICRC2TokenActor(icrc2TokenActor)) {
        await icrc2TokenActor.icrc1_fee();
      };
    };
  };
};
