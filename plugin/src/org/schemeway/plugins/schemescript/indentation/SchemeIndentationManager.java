/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.indentation;

import java.util.*;

import org.eclipse.core.runtime.*;
import org.eclipse.jface.preference.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.preferences.*;

public class SchemeIndentationManager {
    private static IndentationRule DEFAULT_SCHEME;
    static {
        DEFAULT_SCHEME = new IndentationRule(null, IndentationRule.DEFAULT, 0);
    }

    private Map<String, IndentationRule> mRulesMap;

    public SchemeIndentationManager() {
        mRulesMap = new HashMap<String, IndentationRule>();
    }

    public void clear() {
        mRulesMap.clear();
    }

    public void setRules(IndentationRule[] rules) {
        Assert.isNotNull(rules);

        clear();
        for (int index = 0; index < rules.length; index++) {
            IndentationRule scheme = rules[index];
            if (scheme != null) {
            	addIndentationRule(scheme);
            }
        }
    }
    
    public IndentationRule[] getRules() {
    	return (IndentationRule[]) mRulesMap.values().toArray(new IndentationRule[mRulesMap.size()]);
    }

    public void addIndentationRule(IndentationRule rule) {
    	mRulesMap.put(rule.getSymbol(), rule);
    }
    
    public final IndentationRule getFunction(String symbol) {
        IndentationRule scheme = (IndentationRule) mRulesMap.get(symbol);
        if (scheme == null)
            return DEFAULT_SCHEME;
        else
            return scheme;
    }

    public void loadRules() {
        IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
        setRules(PreferenceUtil.getIndentationSchemes(store, IndentationPreferences.INDENT_SCHEMES));
    }
    
    public void saveRules() {
    	IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
    	PreferenceUtil.setIndentationSchemes(store, IndentationPreferences.INDENT_SCHEMES, getRules());
    }

    /* -- Factory methods -- */

}