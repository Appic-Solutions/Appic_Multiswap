import Hash "mo:base/Hash";
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";

actor TokenCanister {
  // A hashmap to store each user's balance. The key is user's Principal ID, and the value is their token balance.
  private var balances : HashMap.HashMap<Principal, Nat> = HashMap.HashMap<Principal, Nat>(10, Principal.hash, Nat.equal);

  // A function to initialize or update a user's balance.
  public func setBalance(newBalance : Nat) : async () {
    let caller = Principal.fromActor(this);
    balances.put(caller, newBalance);
  };

  // A function to retrieve the balance of the caller.
  public func getBalance() : async Nat {
    let caller = Principal.fromActor(this);
    switch (balances.get(caller)) {
      case (null) { return 0 };
      case (?balance) { return balance };
    };
  };

  // A function to transfer tokens from the caller to another user.
  public func transferTokens(recipient : Principal, amount : Nat) : async Bool {
    let caller = Principal.fromActor(this);
    let callerBalance = switch (balances.get(caller)) {
      case (null) { 0 };
      case (?balance) { balance };
    };
    if (callerBalance >= amount) {
      // Subtract the amount from the caller's balance.
      balances.put(caller, callerBalance - amount);

      // Add the amount to the recipient's balance.
      let recipientBalance = switch (balances.get(recipient)) {
        case (null) { amount };
        case (?balance) { balance + amount };
      };
      balances.put(recipient, recipientBalance);
      return true;
    } else {
      return false; // Insufficient funds
    };
  };
};
