/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.indentation;

public final class IndentationRule {
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
    private String mCategory;
    private int mHint;

    public IndentationRule(String symbol, String category, int hint) {
        mSymbol = symbol;
        setCategory(category);
        setHint(hint);
    }

    public String getSymbol() {
        return mSymbol;
    }

    public String getCategory() {
        return mCategory;
    }

    public int getHint() {
        return mHint;
    }

    public final void setCategory(String category) {
        mCategory = category;
    }

    public final void setHint(int hint) {
        mHint = hint;
    }
}