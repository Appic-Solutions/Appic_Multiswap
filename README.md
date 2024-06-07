# AppIC_multiswap

![technical Architechture of multiswap](Multiswap.png)

# Overview of the Code Structure

The code provided is an actor named `Appic_Multiswap`, which contains various functions to facilitate the transfer and swapping of tokens. The actor includes multiple types of token actors such as `TokenActor`, `ICRC1TokenActor`, and `ICRC2TokenActor`, each with its set of functions for token-related operations. Additionally, the actor interacts with a Sonic canister for token swaps and deposits. Here's a detailed breakdown of the provided code:

## Type Definitions and Variables

The actor includes several type definitions such as `TransferReceipt`, `TokenToNum`, `sonicActor`, `TxReceipt`, `Subaccount`, `ICRCAccount`, `ICRCTransferArg`, `ICRC2TransferArg`, and `TokenActorVariable`, along with the definition of variables like `userTokensLocked` and `txcounter`.

## Function Declarations

1. `private func _getTokenActorWithType(tokenId : Text, tokenType : Text) : async TokenActorVariable`

- This function retrieves the specific token actor based on the provided `tokenId` and `tokenType`.

2. `private func _transferFrom(tokenId : Text, tokenType : Text, caller : Principal, value : Nat) : async TransferReceipt`

- Transfers tokens from a specific token actor based on the `tokenId` and `tokenType` provided.

3. `private func _transfer(tokenId : Text, tokenType : Text, caller : Principal, value : Nat) : async TransferReceipt`

- Transfers tokens using a specific token actor based on the `tokenId` and `tokenType` provided.

4. `private func swapTokensWithSonic(sellToken : Text, buyToken : Text, to : Principal, swapAmount : Nat) : async TxReceipt`

- Initiates token swaps using the Sonic canister based on the specified tokens and amounts.

5. `private func transferTokensToCanister(tokenId : Principal, tokenType : Text, caller : Principal, value : Nat) : async TransferReceipt`

- Transfers tokens to the designated canister based on the provided `tokenId` and `tokenType`.

6. `public func withdrawTokens(tokenType : Text, caller : Principal, tokenID : Principal) : async TransferReceipt`

- Allows the withdrawal of tokens based on the specified `tokenType`, `caller`, and `tokenID`.

7. `public func Multiswap(sellingTokens : [Principal], buyingTokens : [Principal], sellAmounts : [Nat], buyAmounts : [Nat], midToken : Principal, midTokenType : Text, sellingTokensType : [Text], buyingTokensType : [Text], caller : Principal)`

- Facilitates the multi-token swap process considering multiple parameters for selling and buying tokens, along with the intermediary token and the caller's details.

8. `public query func getAllUserTokens(caller : Principal) : async ([Principal], [Nat])`

- Retrieves all the tokens held by a specific "caller" along with their respective balances.

## Conclusion

The provided code presents a robust implementation for token transfers, swaps, and related operations, catering to different token types and scenarios. Each function is efficiently designed to handle specific token-related tasks and interactions with the Sonic canister.
