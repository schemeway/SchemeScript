/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.parser;

public final class SchemeScannerUtilities {

    private static boolean mBracketsAsParenthesis = true;
    private static boolean mDashInIdentifiers = false;

    public final static int NONE = 0;
    public final static int PARENTHESIS = 1;
    public final static int BRACKET = 2;
    public final static int BRACE = 3;

    public final static int getParenthesisType(char ch) {
        switch (ch) {
            case '(':
            case ')':
                return PARENTHESIS;
            case '[':
            case ']':
                if (mBracketsAsParenthesis)
                    return BRACKET;
                else
                    return NONE;
            default:
                return NONE;
        }
    }

    public static final boolean bracketsAreParentheses() {
        return mBracketsAsParenthesis;
    }
    
    public static final boolean dashInIdentifiers() {
        return mDashInIdentifiers;
    }
    
    public static final void setBracketsAreParentheses(boolean value) {
        mBracketsAsParenthesis = value;
    }

    public static final void setDashInIdentifiers(boolean value) {
        mDashInIdentifiers = value;
    }

    public final static boolean isParenthesis(char ch) {
        return getParenthesisType(ch) != NONE;
    }

    public final static boolean isOpeningParenthesis(char ch) {
        return (ch == '(' || (mBracketsAsParenthesis && (ch == '[' || ch == '{')));
    }

    public final static boolean isClosingParenthesis(char ch) {
        return (ch == ')' || (mBracketsAsParenthesis && (ch == ']' || ch == '}')));
    }

    public final static boolean isPunctuationChar(char ch) {
        return (isParenthesis(ch) || ch == '\'' || ch == ',' || ch == '`');
    }

    public final static boolean isIdentifierPrefixChar(char ch) {
        return (Character.isLetter(ch) || isSpecialInitial(ch));
    }

    public final static boolean isIdentifierPartChar(char ch) {
        return (Character.isDigit(ch) || isIdentifierPrefixChar(ch) || isSpecialSubsequent(ch));
    }

    public final static boolean isSpecialInitial(char ch) {
        return (ch == '!'
                || ch == '$'
                || ch == '%'
                || ch == '&'
                || ch == '*'
                || ch == '/'
                || ch == ':'
                || ch == '<'
                || ch == '='
                || ch == '>'
                || ch == '?'
                || ch == '^'
                || ch == '_'
                || ch == '~' 
                || (ch == '#' && mDashInIdentifiers));
    }

    public final static boolean isSpecialSubsequent(char ch) {
        return ch == '-'
               || ch == '+'
               || ch == '.'
               || ch == '@'
               || (!mBracketsAsParenthesis && (ch == '[' || ch == ']'));
    }

    public final static boolean isWhitespaceChar(char ch) {
        return (ch == ' ' || ch == '\n' || ch == '\t' || ch == '\r');
    }

    public final static boolean isIdentifier(String str) {
        if (str == null || str.equals(""))
            return false;
        for (int i = 0; i < str.length(); i++) {
            if (!isIdentifierPartChar(str.charAt(i))) {
                return false;
            }
        }
        return true;
    }
}