# AppIC_multiswap

![technical Architechture of multiswap](Multiswap.png)

- `getUserData(user : Principal) : UserData` - Retrieves the user's data from the balances map. If no data exists for the user, initializes and returns default user data.
- `transferTokens(tokenCanister : TokenActorVariant, caller : Principal, value : Nat) : async TransferReceipt` - Performs token transfer based on the token type provided (e.g., DIP, YC, ICRC1, ICRC2). It adjusts the user's stored balance and total deposited amount accordingly.
- `performDIPTransfer(dipTokenActor : actor { transfer : (Principal, Nat) -> async Nat }, caller : Principal, value : Nat) : async TransferReceipt` - Handles the transfer of tokens for DIP token actors, executing the transfer function and returning a transaction ID.
- `performYCTransfer(ycTokenActor : actor { transfer : (Principal, Nat) -> async Nat }, caller : Principal, value : Nat) : async TransferReceipt` - Executes token transfer for YC token actors, calling the transfer method and returning a transaction ID.
- `performICRCTransfer(icrcTokenActor : actor { icrc1_transfer : (ICRCTransferArg) -> async Nat }, caller : Principal, value : Nat) : async TransferReceipt` - Conducts token transfers for ICRC1 or ICRC2 token actors using a structured transfer argument that includes from/to subaccounts and the transfer amount, returning a transaction ID.
- `getUserTokenData(user : Principal) : async UserData` - Provides a query function that returns the total deposited tokens and the detailed token holdings for a specified user.
