/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.dictionary;

import java.util.*;

import org.eclipse.jface.text.templates.*;

public abstract class AbstractSymbolDictionary implements ISymbolDictionary {
    private AbstractSymbolDictionary mParent;
    
    protected AbstractSymbolDictionary() {
        this(null);
    }
    
    protected AbstractSymbolDictionary(AbstractSymbolDictionary parent) {
        mParent = parent;
    }
    
    public void dispose() {
    }
    
    public AbstractSymbolDictionary getParent() {
        return mParent;
    }
    
    public final SymbolEntry[] findSymbol(String name) {
        List entries = new ArrayList();
        AbstractSymbolDictionary parent = getParent();
        while (parent != null) {
            parent.findSymbols(name, entries);
            parent = parent.getParent();
        }
        findSymbols(name, entries);
        return (SymbolEntry[]) entries.toArray(new SymbolEntry[entries.size()]);
    }

    public final SymbolEntry[] completeSymbol(String prefix) {
        List entries = new ArrayList();
        AbstractSymbolDictionary parent = getParent();
        while (parent != null) {
            parent.completeSymbols(prefix, entries);
            parent = parent.getParent();
        }
        completeSymbols(prefix, entries);
        return (SymbolEntry[]) entries.toArray(new SymbolEntry[entries.size()]);
    }
    
    public Template[] completeTemplates(String prefix) {
        return new Template[0];
    }
    
    public TemplateContextType getTemplateContextType() {
        return null;
    }

    protected abstract void findSymbols(String name, List entries);
    protected abstract void completeSymbols(String prefix, List entries);
}
