'use strict';

exports.nullFFI = function(dictNeverNull) {
    return null;
};

exports.justFFI = function(dictNeverNull) {
    return function(value) {
        return value;
    };
};

exports.nullableFFI = function(onNull) {
    return function(onJust) {
        return function(value) {
            if (value === null) {
                return onNull;
            } else {
                return onJust(value);
            }
        };
    };
};

exports.neverNullFFI = function(value) {
    return function(callback) {
        return callback(null);
    };
};
