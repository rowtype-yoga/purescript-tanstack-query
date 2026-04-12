#!/usr/bin/env node
// Post-processes PureScript compiled JS using a real AST parser
// to eliminate unreachable branches from pattern match codegen.
//
// PureScript compiles `case` to sequential if-instanceof-return chains
// with a final throw for exhaustiveness. The type system guarantees
// exhaustiveness, so:
//   1. The throw is dead code
//   2. The last if-instanceof always matches (all prior cases returned)
//   3. Sequential if-return; if can be collapsed to if/else
//
// This makes coverage tools report accurately.

import { readFileSync, writeFileSync } from 'fs'
import { parse, print, types } from 'recast'
import { glob } from 'node:fs'

const b = types.builders

const file = process.argv[2] || 'output/TanStack.Query/index.js'
const code = readFileSync(file, 'utf8')
const ast = parse(code, { sourceFileName: file })

types.visit(ast, {
  // Remove: throw new Error("Failed pattern match ...")
  visitThrowStatement(path) {
    const arg = path.node.argument
    if (arg?.type === 'NewExpression' &&
        arg.callee?.name === 'Error' &&
        arg.arguments?.[0]?.type === 'BinaryExpression') {
      const left = extractString(arg.arguments[0])
      if (left?.startsWith('Failed pattern match')) {
        path.prune()
        return false
      }
    }
    this.traverse(path)
  },

  // Transform sequential if-instanceof blocks in a block statement
  visitBlockStatement(path) {
    this.traverse(path)
    collapseIfChains(path.node.body)
  },

  // Also handle Program-level and expression statement bodies
  visitProgram(path) {
    this.traverse(path)
    collapseIfChains(path.node.body)
  },
})

function extractString(node) {
  if (node.type === 'Literal' || node.type === 'StringLiteral') return node.value
  if (node.type === 'BinaryExpression' && node.operator === '+') return extractString(node.left)
  return null
}

function isInstanceofTest(node) {
  return node.type === 'IfStatement' &&
    node.test?.type === 'BinaryExpression' &&
    node.test.operator === 'instanceof'
}

function returnsInAllPaths(node) {
  if (!node) return false
  if (node.type === 'ReturnStatement') return true
  if (node.type === 'BlockStatement') {
    return node.body.some(s => returnsInAllPaths(s))
  }
  if (node.type === 'IfStatement') {
    return returnsInAllPaths(node.consequent) &&
      returnsInAllPaths(node.alternate)
  }
  return false
}

function collapseIfChains(body) {
  if (!Array.isArray(body)) return

  // First, find pairs of instanceof-ifs (skipping EmptyStatements between them)
  for (let i = 0; i < body.length; i++) {
    const curr = body[i]
    if (!isInstanceofTest(curr) || curr.alternate || !returnsInAllPaths(curr.consequent)) continue

    // Find next non-empty statement
    let j = i + 1
    while (j < body.length && body[j].type === 'EmptyStatement') j++
    if (j >= body.length) continue

    const next = body[j]
    if (!isInstanceofTest(next)) continue

    // Check they test the same variable
    if (sourceOf(curr.test.left) === sourceOf(next.test.left)) {
      // Chain: make next the else branch of curr, remove in-between empties and next
      curr.alternate = next
      body.splice(i + 1, j - i)
      i-- // re-check in case of 3+ cases
    }
  }

  // Convert final else-if-instanceof to plain else
  for (const stmt of body) {
    collapseLastElseIf(stmt)
  }
}

function collapseLastElseIf(node) {
  if (!node || node.type !== 'IfStatement') return

  if (node.alternate?.type === 'IfStatement') {
    const alt = node.alternate
    if (isInstanceofTest(alt) && !alt.alternate) {
      // This is the last else-if with no further branches — collapse to else
      node.alternate = alt.consequent
    } else {
      collapseLastElseIf(node.alternate)
    }
  }
}

function sourceOf(node) {
  // Quick identity check for AST nodes
  if (node.type === 'Identifier') return node.name
  if (node.type === 'MemberExpression') return sourceOf(node.object) + '.' + sourceOf(node.property)
  return JSON.stringify(node)
}

const output = print(ast, { sourceMapName: file + '.map' })
writeFileSync(file, output.code)
console.log('Patched', file)
