/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import java.util.*;

import org.eclipse.core.resources.*;
import org.eclipse.ui.*;
import org.eclipse.ui.part.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.dictionary.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.views.*;

public class JumpToDefinitionAction extends SchemeAction {
    
    public JumpToDefinitionAction(SchemeEditor editor) {
        super(editor);
        setText("Find definition");
        setToolTipText("Jump to the symbol definition");
    }

    private IResource getResource() {
        IEditorInput input = getSchemeEditor().getEditorInput();
        if (input instanceof FileEditorInput) {
            return ((FileEditorInput) input).getFile();
        }
        return null;
    }

    public void run() {
        final SchemeEditor editor = getSchemeEditor();
        if (editor == null) return;

        try {
            String symbol = SchemeTextUtilities.findSymbolAroundPoint(editor.getDocument(), editor.getPoint());
            if (symbol == null)
                return;

            ISymbolDictionary dictionary = editor.getSymbolDictionary();
            if (dictionary == null)
                return;

            SymbolEntry[] entries = dictionary.findSymbol(symbol);
            if (entries.length > 0) {
                SymbolEntry entry = null;
                if (entries.length == 1) {
                    entry = entries[0];
                }
                else {
                    entries = boostPriorities(entries);
                    DefinitionListView.showInView(entries);
                }

                if (entry != null && entry.getFile() == null)
                    entry = null;

                if (entry != null) {
                    DefinitionListView.openEditorAtLine(entry);
                }
            }
        }
        catch (Throwable exception) {
            SchemeScriptPlugin.logException("Exception in jump definition", exception);
        }
    }

    private SymbolEntry[] boostPriorities(SymbolEntry[] entries) {
        List list = Arrays.asList(entries);

        Collections.sort(list, new Comparator() {
            public int compare(Object o1, Object o2) {
                SymbolEntry e1 = (SymbolEntry) o1;
                SymbolEntry e2 = (SymbolEntry) o2;
                int p1 = e1.getPriority();
                int p2 = e2.getPriority();
                if (e1.getFile() != null && e1.getFile().equals(getResource()))
                    p1 += 10;
                if (e2.getFile() != null && e2.getFile().equals(getResource()))
                    p2 += 10;
                if (p1 < p2)
                    return 1;
                if (p1 == p2)
                    return 0;
                else
                    return -1;
            }
        });

        return (SymbolEntry[]) list.toArray(new SymbolEntry[list.size()]);
    }
}