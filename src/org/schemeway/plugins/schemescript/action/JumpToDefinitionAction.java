/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.core.resources.*;
import org.eclipse.jface.action.*;
import org.eclipse.jface.text.*;
import org.eclipse.ui.*;
import org.eclipse.ui.ide.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.dictionary.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.parser.*;

public class JumpToDefinitionAction extends Action {
    private SchemeEditor mEditor;

    public JumpToDefinitionAction(SchemeEditor editor) {
        setText("Jump to the symbol definition");
        setToolTipText("Jump to the symbol definition");
        mEditor = editor;
    }

    public void run() {
        String symbol = findSymbolNearPoint();
        if (symbol == null)
            return;

        ISymbolDictionary dictionary = SchemeScriptPlugin.getDefault().getDictionary();
        SymbolEntry[] entries = dictionary.findSymbol(symbol);
        if (entries.length > 0) {
            SymbolEntry entry = null;
            for(int i=0; i<entries.length; i++) {
                if (entries[i].getMarker() != null) {
                    entry = entries[i];
                    break;
                }
            }
            if (entry != null) {
                IMarker marker = entry.getMarker();
                try {
                    IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
                    IDE.openEditor(page, marker);
                }
                catch (PartInitException exception) {
                }
            }
        }
    }

    private String findSymbolNearPoint() {
        IDocument document = mEditor.getDocument();
        int point = mEditor.getPoint();
        int max = document.getLength();
        int start = point;
        int end = point;
        try {
            while (start > 0 && SchemeScannerUtilities.isIdentifierPartChar(document.getChar(start - 1)))
                start--;
            while (end < max && SchemeScannerUtilities.isIdentifierPartChar(document.getChar(end)))
                end++;
            if (start != end)
                return document.get(start, end - start);
            else
                return null;
        }
        catch (BadLocationException exception) {
            return null;
        }
    }
}