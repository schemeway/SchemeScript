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
	
	private Map<String, List> mSymbolReferences = Collections.synchronizedMap(new HashMap<String, List>());

	public void addEntry(String symbol, IResource resource, int offset, int length) {
		List<Reference> references = (List) mSymbolReferences.get(symbol);

		if (references == null) {
			references = new ArrayList<Reference>();
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
		
		List<Reference> filteredReferences = Collections.synchronizedList(new ArrayList<Reference>());
		for (Iterator iter = references.iterator(); iter.hasNext();) {
			Reference reference = (Reference) iter.next();
			if (resource == null || reference.resource.equals(resource)) {
				filteredReferences.add(reference);
			}
		}
		return (Reference[]) filteredReferences.toArray(new Reference[filteredReferences.size()]);
	}
	
	public Reference[] getReferences(String symbol, IResource resource, int startOffset, int endOffset) {
		// TODO: share code with the previous method
		List references = (List) mSymbolReferences.get(symbol);
		if (references == null) {
			return new Reference[0];
		}
		
		List filteredReferences = Collections.synchronizedList(new ArrayList());
		for (Iterator iter = references.iterator(); iter.hasNext();) {
			Reference reference = (Reference) iter.next();
			if (resource == null || reference.resource.equals(resource)) {
				int referenceStart = reference.offset;
				if (startOffset <= referenceStart && referenceStart <= endOffset) {
					filteredReferences.add(reference);
				}
			}
		}
		return (Reference[]) filteredReferences.toArray(new Reference[filteredReferences.size()]);
		
	}
	
	public synchronized void removeReferences(IResource resource) {
		Map<String, List> remainingReferences = new HashMap<String, List>();
		
		Set<String> keyset = mSymbolReferences.keySet();
		for (Iterator<String> iter = keyset.iterator(); iter.hasNext();) {
			String symbol = (String) iter.next();
			removeReferences(remainingReferences, symbol, resource);
		}
		
		mSymbolReferences = Collections.synchronizedMap(remainingReferences);
	}
	
	private void removeReferences(Map<String, List> remainingReferences, String symbol, IResource resource) {
		List references = (List) mSymbolReferences.get(symbol);
		if (references == null) {
			return;
		}
		
		List<Reference> filteredReferences = new ArrayList<Reference>();
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
