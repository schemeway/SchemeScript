/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.core.resources.*;
import org.eclipse.jface.action.*;
import org.eclipse.jface.text.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;
import org.eclipse.ui.ide.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.dialogs.*;
import org.schemeway.plugins.schemescript.dictionary.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.parser.*;

public class JumpToDefinitionAction extends Action {
    private SchemeEditor mEditor;

    public JumpToDefinitionAction(SchemeEditor editor) {
        setText("Find definition");
        setToolTipText("Jump to the symbol definition");
        mEditor = editor;
    }

    public void run() {
        try {
            String symbol = SchemeTextUtilities.findSymbolAroundPoint(mEditor.getDocument(), mEditor.getPoint());
            if (symbol == null)
                return;

            ISymbolDictionary dictionary = mEditor.getSymbolDictionary();
            if (dictionary == null)
                return;

            SymbolEntry[] entries = dictionary.findSymbol(symbol);
            if (entries.length > 0) {
                SymbolEntry entry = null;
                if (entries.length == 1) {
                    entry = entries[0];
                }
                else {
                    Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
                    SymbolSelectionDialog dialog = new SymbolSelectionDialog(shell, entries);
                    dialog.open();
                    entry = dialog.getSelectedEntry();
                }
                
                if (entry != null && entry.getMarker() == null)
                    entry = null;

                if (entry != null) {
                    IMarker marker = entry.getMarker();
                    try {
                        IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
                        IDE.openEditor(page, marker);
                    }
                    catch (PartInitException exception) {
                        SchemeScriptPlugin.logException("Exception in jump definition", exception);
                    }
                }
            }
        }
        catch (Throwable exception) {
            SchemeScriptPlugin.logException("Exception in jump definition", exception);
        }
    }
}