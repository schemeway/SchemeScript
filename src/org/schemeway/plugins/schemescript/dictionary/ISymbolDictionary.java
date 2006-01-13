/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.dictionary;

import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateContextType;

/**
 * A symbol dictionary is a catalog of symbols that should be known
 * to the code completion and "jump to definition" facilities.
 * 
 * @see SymbolEntry
 */
public interface ISymbolDictionary {
    
    void dispose();
    
    /**
     * Finds all entries corresponding to the given symbol
     * @param name the name of the symbol
     * @return an array of symbol entries
     */
    SymbolEntry[] findSymbol(String name);
    
    /**
     * Finds all entries beginning with the given prefix.
     * @param prefix the symbol prefix
     * @return 
     */
    SymbolEntry[] completeSymbol(String prefix);
    
    /**
     * Finds all symbol entries in a given resource
     */
    List findSymbolForResource(IResource resource);
    
    /**
     * Finds all templates beginning with the given prefix 
     */
    Template[] completeTemplates(String prefix);
    TemplateContextType getTemplateContextType();
}
