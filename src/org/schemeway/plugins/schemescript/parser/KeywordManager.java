/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.parser;

import java.util.*;
import java.util.regex.*;

public class KeywordManager {
    public final static String TYPE_DEFINE = "define";
    public final static String TYPE_KEYWORD = "keyword";
    public final static String TYPE_MUTATOR = "mutator";
    public final static String TYPE_SPECIAL = "special";
    public final static String TYPE_CONSTANT = "constant";
    public final static String TYPE_OTHER = "other";

    private Hashtable mSymbols = new Hashtable();
    private KeywordManager mDelegate = null;
    
    private Pattern mDefinePattern = null;
    private Pattern mKeywordPattern = null;
    private Pattern mMutatorPattern = null;
    private Pattern mSpecialPattern = null;
    private Pattern mConstantPattern = null;


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
    
    public void setDefineRegularExpression(String re) {
        if (re == null || "".equals(re))
            mDefinePattern = null;
        else
            mDefinePattern= Pattern.compile(re);
    }

    public void addKeyword(String name) {
        addSymbol(name, TYPE_KEYWORD);
    }
    
    public void setKeywordRegularExpression(String re) {
        if (re == null || "".equals(re))
            mKeywordPattern = null;
        else
            mKeywordPattern = Pattern.compile(re);
    }

    public void addSpecial(String name) {
        addSymbol(name, TYPE_SPECIAL);
    }
    
    public void setSpecialRegularExpression(String re) {
        if (re == null || "".equals(re))
            mSpecialPattern = null;
        else
            mSpecialPattern = Pattern.compile(re);
    }

    public void addMutator(String name) {
        addSymbol(name, TYPE_MUTATOR);
    }
    
    public void setMutatorRegularExpression(String re) {
        if (re == null || "".equals(re))
            mMutatorPattern = null;
        else
            mMutatorPattern = Pattern.compile(re);
    }

    public void addConstant(String name) {
        addSymbol(name, TYPE_CONSTANT);
    }
    
    public void setConstantRegularExpression(String re) {
        if (re == null || "".equals(re))
            mConstantPattern = null;
        else
            mConstantPattern = Pattern.compile(re);
    }

    public String getType(String symbol) {
        Object type = mSymbols.get(symbol);
        if (type == null) {
            if (isDefine(symbol)) return TYPE_DEFINE;
            if (isKeyword(symbol)) return TYPE_KEYWORD;
            if (isSpecial(symbol)) return TYPE_SPECIAL;
            if (isMutator(symbol)) return TYPE_MUTATOR;
            if (isConstant(symbol)) return TYPE_CONSTANT;

            if (mDelegate == null) {
                return TYPE_OTHER;
            }
            return mDelegate.getType(symbol);
        }
        return (String) type;
    }
    
    private boolean matchesRegexp(String text, Pattern pattern) {
        if (pattern == null) {
            return false;
        }
        Matcher matcher = pattern.matcher(text);
        return matcher.matches();
    }
    
    private boolean isDefine(String symbol) {
        return matchesRegexp(symbol, mDefinePattern);
    }
    
    private boolean isKeyword(String symbol) {
        return matchesRegexp(symbol, mKeywordPattern);
    }
    
    private boolean isSpecial(String symbol) {
        return matchesRegexp(symbol, mSpecialPattern);
    }
    
    private boolean isMutator(String symbol) {
        return matchesRegexp(symbol, mMutatorPattern);
    }
    
    private boolean isConstant(String symbol) {
        return matchesRegexp(symbol, mConstantPattern);
    }
}