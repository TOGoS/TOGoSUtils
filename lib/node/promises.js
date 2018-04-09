// This file is maintained as part of NodeBuildUtil: https://github.com/TOGoS/NodeBuildUtil
// If you're making fixes and want to make sure they get merged upstream,
// PR to that project.
// Otherwise, feel free to remove this comment.
///<reference types="node" />
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var State;
(function (State) {
    State[State["NORMAL"] = 0] = "NORMAL";
    State[State["RESOLVED"] = 1] = "RESOLVED";
    State[State["REJECTED"] = 2] = "REJECTED";
})(State || (State = {}));
var STATESYM = Symbol("resolved");
var VALUESYM = Symbol("value");
var ERRORSYM = Symbol("value");
exports.RESOLVED_VOID_PROMISE = resolvedPromise(undefined);
exports.RESOLVED_VOID_PROMISE_CALLBACK = function () { return exports.RESOLVED_VOID_PROMISE; };
function voidify(p) {
    return p.then(exports.RESOLVED_VOID_PROMISE_CALLBACK);
}
exports.voidify = voidify;
function resolvedPromise(value) {
    var p = Promise.resolve(value);
    p[VALUESYM] = value;
    p[STATESYM] = State.RESOLVED;
    return p;
}
exports.resolvedPromise = resolvedPromise;
/**
 * Add a callback to a promise so that once it resolves
 * it can be queried immediately.
 */
function resolveWrap(thenable) {
    var thenableProps = thenable;
    if (thenableProps[STATESYM] == null) {
        thenableProps[STATESYM] = State.NORMAL;
        thenable.then(function (v) {
            thenableProps[STATESYM] = State.RESOLVED;
            thenableProps[VALUESYM] = v;
        }, function (error) {
            thenableProps[STATESYM] = State.REJECTED;
            thenableProps[ERRORSYM] = error;
        });
    }
    return thenable;
}
exports.resolveWrap = resolveWrap;
function rejectedPromise(error) {
    var p = Promise.reject(value);
    p[ERRORSYM] = error;
    p[STATESYM] = State.REJECTED;
    return p;
}
exports.rejectedPromise = rejectedPromise;
function isResolved(p) {
    return p[STATESYM] === State.RESOLVED;
}
exports.isResolved = isResolved;
function isRejected(p) {
    return p[STATESYM] === State.REJECTED;
}
exports.isRejected = isRejected;
function value(p) {
    return (p[VALUESYM]);
}
exports.value = value;
function error(p) {
    return p[ERRORSYM];
}
exports.error = error;
function isThenable(v) {
    return v != null && v.then;
}
function thenable(v) {
    return isThenable(v) ? v : resolvedPromise(v);
}
function shortcutThen(p, onResolve) {
    if (isResolved(p)) {
        var u = onResolve(value(p));
        return isThenable(u) ? u : resolvedPromise(u);
    }
    return p.then(onResolve);
}
exports.shortcutThen = shortcutThen;
/**
 * If p is null, return an immediately resolved promise.
 * Otherwise return p.
 */
function vopToPromise(p, v) {
    return p != null ? p : resolvedPromise(v);
}
exports.vopToPromise = vopToPromise;
function finalmente(p, finalStuff) {
    return p.then(function (v) { finalStuff(); return v; }, function (err) { finalStuff(); return Promise.reject(err); });
}
exports.finalmente = finalmente;
