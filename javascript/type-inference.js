var esprima = require('esprima');

var dispatch = {
	Program: function(astNode) {
		return findType(astNode.body[0]);
	},

	ExpressionStatement: function(astNode) {
		return findType(astNode.expression);
	},

	BinaryExpression: function(astNode) {
		if (astNode.operator === '+')
			return plusType(findType(astNode.left), findType(astNode.right));
		else
			return undefined;
	},

	FunctionDeclaration: function(astNode) {
		var domainType;
		if (astNode.params.length == 0)
			domainType = 'void';
		else if (astNode.params.length > 0) {
			domainType = findType(astNode.params[0]);
		}

		var returnType = findType(astNode.body);

		return domainType + ' -> ' + returnType;
	},

	FunctionExpression: function(astNode) {
		return this.FunctionDeclaration(astNode);
	},

	CallExpression: function(astNode) {
		var functionType = findType(astNode.callee);
		var argType = findType(astNode.arguments[0]);
		return callType(functionType, argType);
	},

	Identifier: function(astNode) {
		return undefined;
	},

	BlockStatement: function(astNode) {
		return findType(astNode.body[astNode.body.length-1]);
	},

	ReturnStatement: function(astNode) {
		return findType(astNode.argument);
	},

	Literal: function(astNode) {
		return typeof astNode.value;
	},

};

function callType(fnType, argType) {
	return fnType.split('->')[1].trim();
}

function plusType(e1, e2) {
	if (e1 === 'string' || e2 === 'string')
		return 'string';
	else
		return 'number';
}

function findType(term) {
	var type = dispatch[term.type](term);
	if (type == null)
		return '?';
	else
		return type;
}

function type(program) {
	return findType(esprima.parse(program));
}

console.log(type('"blah"'), 'string');
console.log(type('1'), 'number');
console.log(type('1 + 1'), 'number');
console.log(type('1 + "blah"'), 'string');
console.log(type('function bla() { return 1; }'), 'void -> number');
console.log(type('function bla(x) { return 1; }'), '? -> number');
console.log(type('(function bla(x) { return 1; })("bluh")'), 'number');
console.log(type('function bla(x) { return x; }'), '? -> ?');
console.log(type('(function bla(x) { return x; })("bluh")'), 'string');
console.log(type('(function bla(x) { return x; })(12)'), 'number');
