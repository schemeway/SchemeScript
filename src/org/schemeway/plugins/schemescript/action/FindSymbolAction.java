/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.action.*;
import org.eclipse.jface.util.*;
import org.eclipse.jface.window.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;
import org.schemeway.plugins.schemescript.dialogs.*;
import org.schemeway.plugins.schemescript.dictionary.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.views.*;

public class FindSymbolAction extends Action {

    private SchemeEditor mEditor;

    public FindSymbolAction(SchemeEditor editor) {
        Assert.isNotNull(editor);
        setText("Choose symbol");
        setToolTipText("Choose symbol");
        mEditor = editor;
    }
    
    public void run() {
        Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
        FindSymbolDialog dialog = new FindSymbolDialog(shell, mEditor.getSymbolDictionary());
        if (dialog.open() == Window.OK) {
            SymbolEntry[] entries = dialog.getSelectedEntries();
            if (entries != null) {
                if (entries.length == 1)
                    DefinitionListView.openEditorAtLine(entries[0]);
                else
                    DefinitionListView.showInView(entries);
            }
        }
    }
}
