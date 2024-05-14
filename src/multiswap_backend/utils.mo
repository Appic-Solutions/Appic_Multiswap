import Blob "mo:base/Blob";
module {

    public func defaultSubAccount() : Blob {
        var index : Nat8 = 0;
        return Blob.fromArray([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, index]);
    };
};
