/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.dictionary;

import java.util.*;

import org.eclipse.core.resources.*;

/**
 * @author SchemeWay Project.
 * 
 */
public class SymbolReferencesTable {
	
	private Map mSymbolReferences = Collections.synchronizedMap(new HashMap());

	public void addEntry(String symbol, IResource resource, int offset, int length) {
		List references = (List) mSymbolReferences.get(symbol);

		if (references == null) {
			references = new ArrayList();
			mSymbolReferences.put(symbol, references);
		}

		references.add(new Reference(resource, offset, length));
	}

	public Reference[] getReferences(String symbol) {
		List references = (List) mSymbolReferences.get(symbol);
		if (references == null) {
			return new Reference[0];
		}
		
		return (Reference[]) references.toArray(new Reference[references.size()]);
	}
	
	public Reference[] getReferences(String symbol, IResource resource) {
		List references = (List) mSymbolReferences.get(symbol);
		if (references == null) {
			return new Reference[0];
		}
		
		List filteredReferences = Collections.synchronizedList(new ArrayList());
		for (Iterator iter = references.iterator(); iter.hasNext();) {
			Reference reference = (Reference) iter.next();
			if (resource == null || reference.resource.equals(resource)) {
				filteredReferences.add(reference);
			}
		}
		return (Reference[]) filteredReferences.toArray(new Reference[filteredReferences.size()]);
	}
	
	public synchronized void removeReferences(IResource resource) {
		Map remainingReferences = new HashMap();
		
		Set keyset = mSymbolReferences.keySet();
		for (Iterator iter = keyset.iterator(); iter.hasNext();) {
			String symbol = (String) iter.next();
			removeReferences(remainingReferences, symbol, resource);
		}
		
		mSymbolReferences = Collections.synchronizedMap(remainingReferences);
	}
	
	private void removeReferences(Map remainingReferences, String symbol, IResource resource) {
		List references = (List) mSymbolReferences.get(symbol);
		if (references == null) {
			return;
		}
		
		List filteredReferences = new ArrayList();
		for (Iterator iter = references.iterator(); iter.hasNext();) {
			Reference reference = (Reference) iter.next();
			if (resource == null || !reference.resource.equals(resource)) {
				filteredReferences.add(reference);
			}
		}
		
		if (filteredReferences.size() != 0) {
			remainingReferences.put(symbol, filteredReferences);
		}
	}
}
