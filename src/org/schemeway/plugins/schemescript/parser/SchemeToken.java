/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.parser;

public final class SchemeToken {
    public final static int EOFTOK = -1;
    public final static int ERROR = 0;
    public final static int LPAREN = 1;
    public final static int RPAREN = 2;
    public final static int SYMBOL = 3;
    public final static int CONSTANT = 4;
    public final static int STRING = 5;
    public final static int SPECIAL = 6;
    public final static int DEFAULT = 7;
    public final static int WSPACE = 8;
    public final static int VECTORPREFIX = 9;
    public final static int QUOTE = 10;
    public final static int BACKQUOTE = 11;
    public final static int UNQUOTE = 12;
    public final static int UNQUOTE_SPLICING = 13;

    public final static SchemeToken EOF = new SchemeToken(EOFTOK, -1, -1);

    private int mType;
    private int mOffset;
    private int mLength;

    protected SchemeToken(int type, int offset, int length) {
        mType = type;
        mOffset = offset;
        mLength = length;
    }

    public int getType() {
        return mType;
    }

    public int getOffset() {
        return mOffset;
    }

    public int getLength() {
        return mLength;
    }

    /* --- Token factory methods --- */

    public static SchemeToken createLeftParen(int offset) {
        return new SchemeToken(LPAREN, offset, 1);
    }

    public static SchemeToken createRightParen(int offset) {
        return new SchemeToken(RPAREN, offset, 1);
    }

    public static SchemeToken createWhitespace(int offset, int length) {
        return new SchemeToken(WSPACE, offset, length);
    }

    public static SchemeToken createConstant(int offset, int length) {
        return new SchemeToken(CONSTANT, offset, length);
    }
    
    public static SchemeToken createSymbol(int offset, int length) {
        return new SchemeToken(SYMBOL, offset, length);
    }
    
    public static SchemeToken createVectorPrefix(int offset) {
        return new SchemeToken(VECTORPREFIX, offset, 1);
    }

    public static SchemeToken createQuote(int offset) {
        return new SchemeToken(QUOTE, offset, 1);
    }

    public static SchemeToken createBackquote(int offset) {
        return new SchemeToken(QUOTE, offset, 1);
    }

    public static SchemeToken createUnquote(int offset) {
        return new SchemeToken(UNQUOTE, offset, 1);
    }

    public static SchemeToken createUnquoteSplicing(int offset) {
        return new SchemeToken(UNQUOTE_SPLICING, offset, 2);
    }

    public static SchemeToken createDefault(int offset, int length) {
        return new SchemeToken(DEFAULT, offset, length);
    }

    public static SchemeToken createString(int offset, int length) {
        return new SchemeToken(STRING, offset, length);
    }

    public static SchemeToken createSpecial(int offset, int length) {
        return new SchemeToken(SPECIAL, offset, length);
    }

    public static SchemeToken createError(int offset, int length) {
        return new SchemeToken(ERROR, offset, length);
    }

    public String toString() {
        return "#[SchemeToken: '" + mType + "' at " + mOffset + "]";
    }
}