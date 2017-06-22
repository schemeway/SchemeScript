/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.parser;

import java.util.*;
import java.util.regex.*;

import org.eclipse.jface.preference.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.preferences.*;

public final class KeywordManager {
	public final static String TYPE_DEFINE = "define";
	public final static String TYPE_KEYWORD = "keyword";
	public final static String TYPE_MUTATOR = "mutator";
	public final static String TYPE_SPECIAL = "special";
	public final static String TYPE_CONSTANT = "constant";
	public final static String TYPE_OTHER = "other";

	private Set<String> mDefines = new HashSet<String>();
	private Set<String> mKeywords = new HashSet<String>();
	private Set<String> mMutators = new HashSet<String>();
	private Set<String> mSpecials = new HashSet<String>();
	private Set<String> mConstants = new HashSet<String>();

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
		mDefines.clear();
		mKeywords.clear();
		mMutators.clear();
		mSpecials.clear();
		mConstants.clear();
	}
	
	private void setNames(String[] names, Set<String> set) {
		for (int index = 0; index < names.length; index++) {
			set.add(names[index]);
		}
	}

	public void addDefine(String name) {
		mDefines.add(name);
	}
	
	public String[] getDefines() {
		return (String[]) mDefines.toArray(new String[mDefines.size()]);
	}
	
	public void setDefines(String[] names) {
		setNames(names, mDefines);
	}
	
	public String getDefineRE() {
		return mDefinePattern == null ? "" : mDefinePattern.pattern();
	}

	public void setDefineRegularExpression(String re) {
		if (re == null || "".equals(re))
			mDefinePattern = null;
		else
			mDefinePattern = Pattern.compile(re);
	}

	public void addKeyword(String name) {
		mKeywords.add(name);
	}
	
	public String[] getKeywords() {
		return (String[]) mKeywords.toArray(new String[mKeywords.size()]);
	}
	
	public void setKeywords(String[] names) {
		setNames(names, mKeywords);
	}
	
	public String getKeywordRE() {
		return mKeywordPattern == null ? "" : mKeywordPattern.pattern();
	}

	public void setKeywordRegularExpression(String re) {
		if (re == null || "".equals(re))
			mKeywordPattern = null;
		else
			mKeywordPattern = Pattern.compile(re);
	}

	public void addSpecial(String name) {
		mSpecials.add(name);
	}
	
	public String[] getSpecials() {
		return (String[]) mSpecials.toArray(new String[mSpecials.size()]);
	}
	
	public void setSpecials(String[] names) {
		setNames(names, mSpecials);
	}

	public String getSpecialsRE() {
		return mSpecialPattern == null ? "" : mSpecialPattern.pattern();
	}
	
	public void setSpecialRegularExpression(String re) {
		if (re == null || "".equals(re))
			mSpecialPattern = null;
		else
			mSpecialPattern = Pattern.compile(re);
	}

	public void addMutator(String name) {
		mMutators.add(name);
	}
	
	public String[] getMutators() {
		return (String[]) mMutators.toArray(new String[mMutators.size()]);
	}
	
	public void setMutators(String[] names) {
		setNames(names, mMutators);
	}
	
	public String getMutatorsRE() {
		return mMutatorPattern == null ? "" : mMutatorPattern.pattern();
	}

	public void setMutatorRegularExpression(String re) {
		if (re == null || "".equals(re))
			mMutatorPattern = null;
		else
			mMutatorPattern = Pattern.compile(re);
	}

	public void addConstant(String name) {
		mConstants.add(name);
	}
	
	public String[] getConstants() {
		return (String[]) mConstants.toArray(new String[mConstants.size()]);
	}
	
	public void setConstants(String[] names) {
		setNames(names, mConstants);
	}

	public String getConstantsRE() {
		return mConstantPattern == null ? "" : mConstantPattern.pattern();
	}
	
	public void setConstantRegularExpression(String re) {
		if (re == null || "".equals(re))
			mConstantPattern = null;
		else
			mConstantPattern = Pattern.compile(re);
	}

	public String getType(String symbol) {
		if (isDefine(symbol))
			return TYPE_DEFINE;
		if (isKeyword(symbol))
			return TYPE_KEYWORD;
		if (isSpecial(symbol))
			return TYPE_SPECIAL;
		if (isMutator(symbol))
			return TYPE_MUTATOR;
		if (isConstant(symbol))
			return TYPE_CONSTANT;

		if (mDelegate == null) {
			return TYPE_OTHER;
		}
		return mDelegate.getType(symbol);

	}

	private boolean matchesRegexp(String text, Pattern pattern) {
		if (pattern == null) {
			return false;
		}
		Matcher matcher = pattern.matcher(text);
		return matcher.matches();
	}

	private boolean isDefine(String symbol) {
		return mDefines.contains(symbol) || matchesRegexp(symbol, mDefinePattern);
	}

	private boolean isKeyword(String symbol) {
		return mKeywords.contains(symbol) || matchesRegexp(symbol, mKeywordPattern);
	}

	private boolean isSpecial(String symbol) {
		return mSpecials.contains(symbol) || matchesRegexp(symbol, mSpecialPattern);
	}

	private boolean isMutator(String symbol) {
		return mMutators.contains(symbol) || matchesRegexp(symbol, mMutatorPattern);
	}

	private boolean isConstant(String symbol) {
		return mConstants.contains(symbol) || matchesRegexp(symbol, mConstantPattern);
	}
	
	public void saveValues() {
		IPreferenceStore store = SchemeScriptPlugin.getDefault().getPreferenceStore();
        PreferenceUtil.setKeywords(store, SyntaxPreferences.SYNTAX_DEFINE, getDefines());
        store.setValue(SyntaxPreferences.SYNTAX_DEFINE_RE, getDefineRE());
        PreferenceUtil.setKeywords(store, SyntaxPreferences.SYNTAX_KEYWORD, getKeywords());
        store.setValue(SyntaxPreferences.SYNTAX_KEYWORD_RE, getKeywordRE());
        PreferenceUtil.setKeywords(store, SyntaxPreferences.SYNTAX_SPECIAL, getSpecials());
        store.setValue(SyntaxPreferences.SYNTAX_SPECIAL_RE, getSpecialsRE());
        PreferenceUtil.setKeywords(store, SyntaxPreferences.SYNTAX_MUTATOR, getMutators());
        store.setValue(SyntaxPreferences.SYNTAX_MUTATOR_RE, getMutatorsRE());
        PreferenceUtil.setKeywords(store, SyntaxPreferences.SYNTAX_CONSTANT, getConstants());
        store.setValue(SyntaxPreferences.SYNTAX_CONSTANT_RE, getConstantsRE());
		
	}
}