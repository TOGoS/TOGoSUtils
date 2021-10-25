
import { assertEquals } from "https://deno.land/std@0.113.0/testing/asserts.ts";

Deno.test("regex replace works", () => {
	const input = "foo bar baz";
	const result = input.replace( /(.)ar/, "$1az");
	assertEquals(result, "foo baz baz");
});

Deno.test("regex backreference", () => {
	const input = "foo bar baz";
	const result = input.replace( /b(.)r b\1z/, "b$1x-b$1x");
	assertEquals(result, "foo bax-bax");
});

Deno.test("parsed regex backreference", () => {
	const input = "foo bar baz";
    const regexString = "b(.)r b\\1z";
    const regex = new RegExp(regexString);
	const result = input.replace( regex, "b$1x-b$1x");
	assertEquals(result, "foo bax-bax");
});
