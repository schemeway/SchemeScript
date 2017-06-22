/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.dictionary;

import gnu.lists.*;
import gnu.mapping.*;

import java.util.*;

import org.eclipse.core.resources.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.interpreter.*;

/**
 * @author SchemeWay Project.
 * 
 */
public final class DictionaryUtils {
	
	public static SymbolEntry[] findUserDefinitions(String symbol) {
		Object object = KawaProxy.get("get-dictionary-entries");
		if (object != null && object instanceof Procedure) {
			Procedure getDictionaryEntries = (Procedure) object;
			try {
				LList entryList = (LList) getDictionaryEntries.apply1(symbol);
				List<Object> entries = new ArrayList<Object>();
				addEntriesToJavaList(entryList, entries);
				return (SymbolEntry[]) entries.toArray(new SymbolEntry[entries.size()]);
			}
			catch (Throwable exception) {
				SchemeScriptPlugin.logException("Unable to find definitions for symbol: " + symbol , exception);
			}
		}
		return new SymbolEntry[0];
	}
	
	public static SymbolEntry[] findDefinitionsForResource(IResource resource) {
		
		Object object = KawaProxy.get("get-dictionary-entries-for-resource");
		if (object != null && object instanceof Procedure) {
			Procedure getDictionaryEntries = (Procedure) object;
			try {
				LList entryList = (LList) getDictionaryEntries.apply1(resource);
				List<Object> entries = new ArrayList<Object>();
				addEntriesToJavaList(entryList, entries);
				return (SymbolEntry[]) entries.toArray(new SymbolEntry[entries.size()]);
			}
			catch (Throwable exception) {
				SchemeScriptPlugin.logException("Unable to find definitions for resource: " + resource.getName(), exception);
			}
		}
		return new SymbolEntry[0];
		
	}
	
	public static SymbolEntry[] findCompletions(String prefix) {
		Object object = KawaProxy.get("find-completions");
		if (object != null && object instanceof Procedure)
		{
			Procedure proc = (Procedure) object;
			try {
				LList entryList = (LList) proc.apply1(prefix);
				LinkedList<Object> entries = new LinkedList<Object>();
				addEntriesToJavaList(entryList, entries);
				return (SymbolEntry[]) entries.toArray(new SymbolEntry[entries.size()]);
			}
			catch (Throwable exception) {
				SchemeScriptPlugin.logException("Unable to call completion procedure", exception);
			}
		}
		return new SymbolEntry[0];
	}

	private static void addEntriesToJavaList(LList entryList, List<Object> entries) {
		while (entryList instanceof Pair) {
			Pair pair = (Pair) entryList;
			entries.add(pair.getCar());
			entryList = (LList) pair.getCdr();
		}
	}

	private DictionaryUtils() {
	}

}
