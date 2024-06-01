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

actor Appic_Multiswap {
  type TransferReceipt = {
    #ok : Nat; // Indicates a successful transfer with a transaction ID
    #err : Text; // Indicates an error occurred during the transfer
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

  private stable var txcounter : Nat = 0;
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
          case (#ok(id)) { return #ok(id) };
          case (#err(e)) { return #err(e) };
        };
      };
      case (#ICRC1TokenActor(icrc1TokenActor)) {
        var defaultSubaccount : Blob = Utils.defaultSubAccount();
        var userSubAccount : Subaccount = await getSubAccount();
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
          case (#ok(id)) { return #ok(id) };
          case (#err(e)) { return #err(e) };
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
          case (#ok(id)) { return #ok(id) };
          case (#err(e)) { return #err(e) };
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
          case (#ok(id)) { return #ok(id) };
          case (#err(e)) { return #err(e) };
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
          case (#ok(id)) { return #ok(id) };
          case (#err(e)) { return #err(e) };
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
          case (#ok(id)) { return #ok(id) };
          case (#err(e)) { return #err(e) };
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
      case (#ok(id)) { return #ok(id) };
      case (#err(e)) { return #err("token transfer failed") };
    };
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
      case (#ok(id)) { return #ok(id) };
      case (#err(e)) { return #err("Withdraw token failed") };
    };
    txcounter += 1;
    return #ok(txcounter -1);
  };

  public func swapWithSonic(sellToken : Principal, buyToken : Principal, sellTokenType : Text, sellAmt : Nat) : async Nat {
    let tokenActor : TokenActorVariable = await _getTokenActorWithType(Principal.toText(sellToken), sellTokenType);
    let sonicBalanceOfBuyTokenBeforeTrade = await sonicCanister.balanceOf(Principal.toText(buyToken), Principal.fromActor(Appic_Multiswap));
    switch (tokenActor) {
      case (#DIPtokenActor(dipTokenActor)) {
        let _ = switch (await dipTokenActor.approve(sonicCanisterId, sellAmt)) {
          case (#ok(id)) { #ok(id) };
          case (#err(e)) {
            assert (false);
            #err(e);
          };
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
          amount = sellAmt;
        };
        let _ = switch (await icrc1TokenActor.icrc1_transfer(transferArgs)) {
          case (#ok(id)) { #ok(id) };
          case (#err(e)) {
            assert (false);
            #err(e);
          };
        };
      };
      case (#ICRC2TokenActor(icrc2TokenActor)) {
        let _ = switch (await icrc2TokenActor.icrc2_approve(null, sonicCanisterId, sellAmt)) {
          case (#ok(id)) { #ok(id) };
          case (#err(e)) {
            assert (false);
            #err(e);
          };
        };
      };
    };
    let _ = switch (await sonicCanister.deposit(sellToken, sellAmt)) {
      case (#ok(id)) { #ok(id) };
      case (#err(e)) {
        assert (false);
        #err(e);
      };
    };
    let _ = switch (await swapTokensWithSonic(Principal.toText(sellToken), Principal.toText(buyToken), Principal.fromActor(Appic_Multiswap), sellAmt)) {
      case (#ok(id)) { #ok(id) };
      case (#err(e)) {
        assert (false);
        #err(e);
      };
    };
    let sonicBalanceOfBuyTokenAfterTrade = await sonicCanister.balanceOf(Principal.toText(buyToken), Principal.fromActor(Appic_Multiswap));
    let amountOfBoughtToken = sonicBalanceOfBuyTokenAfterTrade - sonicBalanceOfBuyTokenBeforeTrade;
    let _ = await _WithdrawFromSonic(buyToken, amountOfBoughtToken);
    return amountOfBoughtToken;
  };

  public func principalToBlobICPswap(p : Principal) : async Blob {
    var arr : [Nat8] = Blob.toArray(Principal.toBlob(p));
    var defaultArr : [var Nat8] = Array.init<Nat8>(32, 0);
    defaultArr[0] := Nat8.fromNat(arr.size());
    var ind : Nat = 0;
    while (ind < arr.size() and ind < 32) {
      defaultArr[ind + 1] := arr[ind];
      ind := ind + 1;
    };
    return Blob.fromArray(Array.freeze(defaultArr));
  };

  public func swapWithICPSwap(
    sellToken : Principal,
    buyToken : Principal,
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

        let tokenActor : TokenActorVariable = await _getTokenActorWithType(Principal.toText(sellToken), sellTokenType);
        switch (tokenActor) {
          case (#DIPtokenActor(dipTokenActor)) {
            // Approve SwapPool to transfer tokens on behalf of the user
            let _ = switch (await dipTokenActor.approve(swapPoolCanisterId, sellAmt)) {
              case (#ok(id)) { #ok(id) };
              case (#err(e)) {
                assert (false);
                #err(e);
              };
            };
          };
          case (#ICRC1TokenActor(icrc1TokenActor)) {
            // Transfer tokens to the SwapPool's subaccount
            let subaccount : Blob = await principalToBlobICPswap(Principal.fromActor(Appic_Multiswap));
            let transferArgs : ICRCTransferArg = {
              from_subaccount = null;
              to : ICRCAccount = {
                owner = swapPoolCanisterId;
                subaccount = ?subaccount;
              };
              amount = sellAmt;
            };
            let _ = switch (await icrc1TokenActor.icrc1_transfer(transferArgs)) {
              case (#ok(id)) { #ok(id) };
              case (#err(e)) {
                assert (false);
                #err(e);
              };
            };
          };
          case (#ICRC2TokenActor(icrc2TokenActor)) {
            // Approve SwapPool to transfer tokens on behalf of the user
            let _ = switch (await icrc2TokenActor.icrc2_approve(null, swapPoolCanisterId, sellAmt)) {
              case (#ok(id)) { #ok(id) };
              case (#err(e)) {
                assert (false);
                #err(e);
              };
            };
          };
        };

        // Deposit tokens to the SwapPool
        let depositArgs = {
          fee = 3000;
          token = Principal.toText(sellToken);
          amount = sellAmt;
        };
        let _ = switch (await swapPoolCanister.deposit(depositArgs)) {
          case (#ok(id)) { #ok(id) };
          case (#err(e)) {
            assert (false);
            #err(e);
          };
        };

        // Perform the swap
        let swapArgs = {
          amountIn = Nat.toText(sellAmt);
          zeroForOne = true; // Adjust as necessary based on trade direction
          amountOutMinimum = "0"; // No minimum amount restriction
        };
        let amountOut = switch (await swapPoolCanister.swap(swapArgs)) {
          case (#ok(amountOut)) { amountOut };
          case (#err(e)) {
            assert (false);
            0;
          };
        };

        // Withdraw the bought tokens from SwapPool
        let withdrawArgs = {
          fee = 3000;
          token = Principal.toText(buyToken);
          amount = amountOut;
        };
        let _ = switch (await swapPoolCanister.withdraw(withdrawArgs)) {
          case (#ok(id)) { #ok(id) };
          case (#err(e)) {
            assert (false);
            #err(e);
          };
        };

        return amountOut;
      };
      case (#err(e)) {
        return 0;
      };
    };
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
  public func multiswap(sellingTokens : [Principal], buyingTokens : [Principal], sellAmounts : [Nat], buyAmounts : [Nat], midToken : Principal, midTokenType : Text, sellingTokensType : [Text], buyingTokensType : [Text], caller : Principal) {
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
        case (#ok(id)) { #ok(id) };
        case (#err(e)) {
          assert (false);
          #err(e);
        };
      };
      // Call sonicSwapAmountOut
      let sonicAmountOut : Nat = await sonicSwapAmountOut(sellingTokens[i], midToken, sellAmounts[i]);
      // Call icpSwapAmountOut
      let icpAmountOut : Nat = await icpSwapAmountOut(sellingTokens[i], sellingTokensType[i], midToken, midTokenType, sellAmounts[i]);
      // Compare the results and return the better option
      if (sonicAmountOut > icpAmountOut) {
        let _ = await swapWithSonic(sellingTokens[i], midToken, sellingTokensType[i], sellAmounts[i]);
      } else {
        let _ = await swapWithICPSwap(sellingTokens[i], midToken, sellingTokensType[i], midTokenType, sellAmounts[i]);
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

    var midTokenBal = midTokenBalFin -midTokenBalInit;
    midTokenBal := (1000 -multiswap_fee) * midTokenBal / 1000;
    let feeTrans = midTokenBalFin -(midTokenBalInit +midTokenBal);
    let _ = switch (await withdrawTokens(midTokenType, owner, midToken, feeTrans)) {
      case (#ok(id)) { #ok(id) };
      case (#err(e)) {
        assert (false);
        #err(e);
      };
    };

    for (i in Iter.range(0, buyingTokens.size() -1)) {
      let buyActulAmt = buyAmounts[i] * midTokenBal / 100;
      // Call sonicSwapAmountOut
      let sonicAmountOut : Nat = await sonicSwapAmountOut(midToken, buyingTokens[i], buyActulAmt);
      // Call icpSwapAmountOut
      let icpAmountOut : Nat = await icpSwapAmountOut(midToken, midTokenType, buyingTokens[i], buyingTokensType[i], buyActulAmt);
      // Compare the results and return the better option
      if (sonicAmountOut > icpAmountOut) {
        let amountOfBoughtTokenN = await swapWithSonic(midToken, buyingTokens[i], midTokenType, buyActulAmt);
        let _ = switch (await withdrawTokens(buyingTokensType[i], caller, buyingTokens[i], amountOfBoughtTokenN)) {
          case (#ok(id)) { #ok(id) };
          case (#err(e)) {
            assert (false);
            #err(e);
          };
        };
      } else {
        let amountOfBoughtTokenN = await swapWithICPSwap(midToken, buyingTokens[i], midTokenType, buyingTokensType[i], buyActulAmt);
        let _ = switch (await withdrawTokens(buyingTokensType[i], caller, buyingTokens[i], amountOfBoughtTokenN)) {
          case (#ok(id)) { #ok(id) };
          case (#err(e)) {
            assert (false);
            #err(e);
          };
        };
      };
    };
  };

  // Withdraw swapped funds from sonic
  private func _WithdrawFromSonic(
    buyToken : Principal,
    amountToWithdraw : Nat,
  ) : async TxReceipt {
    let withdrawResult : TxReceipt = await sonicCanister.withdraw(buyToken, amountToWithdraw);
    return withdrawResult;
  };

  // transfer arg for transferring of tokens
  //  let transferArgs : ICRCTransferArg = {
  //       from_subaccount = null;
  //       to : ICRCAccount = {
  //         owner = AppIC_Multiswap canister id;
  //         subaccount = getSubbaccount();
  //       };
  public shared (msg) func getSubAccount() : async Blob {
    let subAcc : Blob = getICRC1SubAccount(msg.caller);
    return subAcc;
  };

  private func getICRC1SubAccount(caller : Principal) : Blob {
    let subaccount = Utils.generateSubaccount({
      caller = caller;
      id = 1000;
    });
    return subaccount;
  };

  public shared (msg) func changeFee(val : Nat) : async () {
    assert (msg.caller == owner);
    multiswap_fee := val;
  };
  public shared (msg) func changeOwner(newOwner : Principal) : async () {
    assert (msg.caller == owner);
    owner := newOwner;
  };

  public func withdrawTransferICRC1(tokenPrincipal : Principal) : async () {
    var tokenCanister : TokenActorVariable = await _getTokenActorWithType(Principal.toText(tokenPrincipal), "ICRC1");
    switch (tokenCanister) {
      case (#DIPtokenActor(dipTokenActor)) {
        return;
      };
      case (#ICRC1TokenActor(icrc1TokenActor)) {
        var userSubAccount : Subaccount = await getSubAccount();
        let account : ICRCAccount = {
          owner = Principal.fromActor(Appic_Multiswap);
          subaccount = ?userSubAccount;
        };
        let bal = await icrc1TokenActor.icrc1_balance_of(account);
        if (bal == 0) {
          return;
        } else {
          let getSubbaccount : Blob = Utils.defaultSubAccount();
          let transferArgs : ICRCTransferArg = {
            from_subaccount = null;
            to : ICRCAccount = {
              owner = sonicCanisterId;
              subaccount = ?getSubbaccount;
            };
            amount = bal;
          };
          let _ = switch (await icrc1TokenActor.icrc1_transfer(transferArgs)) {
            case (#ok(id)) { #ok(id) };
            case (#err(e)) {
              #err(e);
            };
          };
        };
      };
      case (#ICRC2TokenActor(icrc2TokenActor)) {
        return;
      };

    };
  };

  public func sonicSwapAmountOut(t0 : Principal, t1 : Principal, amountIn : Nat) : async Nat {
    let ret : (Nat, Nat) = switch (await sonicCanister.getPair(t0, t1)) {
      case (?p) {
        if (p.token0 == Principal.toText(t0)) {
          (p.reserve0, p.reserve1);
        } else {
          (p.reserve1, p.reserve0);
        };
      };
      case (_) {
        return 0;
      };
    };
    let reserveIn = ret.0;
    let reserveOut = ret.1;
    let data = Utils.getAmountOut(amountIn, reserveIn, reserveOut);
    return data.0;
  };

  public func icpSwapAmountOut(token0Address : Principal, token0Standard : Text, token1Address : Principal, token1Standard : Text, amountIn : Nat) : async Nat {
    let token0 = { address = token0Address; standard = token0Standard };
    let token1 = { address = token1Address; standard = token1Standard };

    let poolArgs = { fee = 3000; token0; token1 };

    switch (await swapFactoryCanister.getPool(poolArgs)) {
      case (#ok(poolData)) {
        let swapPoolCanister : SwapPool = actor (Principal.toText(poolData.canisterId));

        // Example swap arguments
        let swapArgs = {
          amountIn = Nat.toText(amountIn);
          zeroForOne = true;
          amountOutMinimum = "0"; // No minimum amount restriction
        };

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
};
