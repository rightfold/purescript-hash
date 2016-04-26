'use strict';

// module Test.Main

exports.isGoodIntImpl = function(bottom) {
    return function(top) {
        return function(n) {
            return n >= bottom && n <= top;
        };
    };
};
