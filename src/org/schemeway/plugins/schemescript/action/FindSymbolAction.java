/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.jface.action.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.jface.window.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;
import org.schemeway.plugins.schemescript.dialogs.*;
import org.schemeway.plugins.schemescript.dictionary.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.views.*;

public class FindSymbolAction extends Action implements IWorkbenchWindowActionDelegate {
    
    IWorkbenchWindow mWindow = null;

    public FindSymbolAction() {
        setText("Choose symbol");
        setToolTipText("Choose symbol");
    }
    
    public void run() {
        Shell shell = null;
        
        if (mWindow == null)
            shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
        else
            shell = mWindow.getShell();
        
        FindSymbolDialog dialog = new FindSymbolDialog(shell, SchemeEditor.getSchemeSymbolDictionary());
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
    
    
    public void dispose() {
    }

    public void init(IWorkbenchWindow window) {
        mWindow = window;
    }

    public void run(IAction action) {
        run();
    }

    public void selectionChanged(IAction action, ISelection selection) {
    }
}
