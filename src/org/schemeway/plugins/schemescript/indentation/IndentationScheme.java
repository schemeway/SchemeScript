/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.indentation;

public final class IndentationScheme {
    public final static String DEFAULT = "default";
    public final static String DEFINITION = "definition";
    public final static String SEQUENCE = "sequence";
    public final static String WITH = "with";
    public final static String IF = "if";
    public final static String NONE = "none";

    public final static String[] ALL_SCHEMES = new String[] {
                DEFAULT, DEFINITION, SEQUENCE, IF, WITH, NONE
    };

    private String mSymbol;
    private String mScheme;
    private int mHint;

    public IndentationScheme(String symbol, String scheme, int hint) {
        mSymbol = symbol;
        setScheme(scheme);
        setHint(hint);
    }

    public String getSymbol() {
        return mSymbol;
    }

    public String getScheme() {
        return mScheme;
    }

    public int getHint() {
        return mHint;
    }

    public final void setScheme(String scheme) {
        mScheme = scheme;
    }

    public final void setHint(int hint) {
        mHint = hint;
    }
}