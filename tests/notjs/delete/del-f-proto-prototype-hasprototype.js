var rec = {__proto__: {foo: 1} }
delete(rec.__proto__)
rec.foo
