/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.parser;

import java.util.*;

public class KeywordManager {
    public final static String TYPE_DEFINE = "define";
    public final static String TYPE_KEYWORD = "keyword";
    public final static String TYPE_MUTATOR = "mutator";
    public final static String TYPE_SPECIAL = "special";
    public final static String TYPE_CONSTANT = "constant";
    public final static String TYPE_OTHER = "other";

    private Hashtable mSymbols = new Hashtable();
    private KeywordManager mDelegate = null;

    public KeywordManager() {
        mDelegate = null;
    }

    public KeywordManager(KeywordManager delegate) {
        mDelegate = delegate;
    }

    public void clear() {
        mSymbols.clear();
    }

    protected void addSymbol(String name, String type) {
        mSymbols.put(name, type);
    }

    public void addDefine(String name) {
        addSymbol(name, TYPE_DEFINE);
    }

    public void addKeyword(String name) {
        addSymbol(name, TYPE_KEYWORD);
    }

    public void addSpecial(String name) {
        addSymbol(name, TYPE_SPECIAL);
    }

    public void addMutator(String name) {
        addSymbol(name, TYPE_MUTATOR);
    }

    public void addConstant(String name) {
        addSymbol(name, TYPE_CONSTANT);
    }

    public String getType(String symbol) {
        Object type = mSymbols.get(symbol);
        if (type == null) {
            if (mDelegate == null) {
                return TYPE_OTHER;
            }
            return mDelegate.getType(symbol);
        }
        return (String) type;
    }
}