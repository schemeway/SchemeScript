/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import java.util.*;
import java.util.List;

import javax.swing.text.*;

import org.eclipse.core.resources.*;
import org.eclipse.jface.action.*;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.window.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;
import org.eclipse.ui.ide.*;
import org.eclipse.ui.part.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.dialogs.*;
import org.schemeway.plugins.schemescript.dictionary.*;
import org.schemeway.plugins.schemescript.editor.*;

public class JumpToDefinitionAction extends Action {
    private SchemeEditor mEditor;
    private IResource mResource;

    public JumpToDefinitionAction(SchemeEditor editor) {
        setText("Find definition");
        setToolTipText("Jump to the symbol definition");
        mEditor = editor;
        setupResource();
    }
    
    private void setupResource() {
        IEditorInput input = mEditor.getEditorInput();
        if (input instanceof FileEditorInput) {
            mResource = ((FileEditorInput)input).getFile();
        }
    }
    
    private IResource getResource() {
        return mResource;
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
                    entries = boostPriorities(entries);
                    Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
                    SymbolSelectionDialog dialog = new SymbolSelectionDialog(shell, entries);
                    dialog.open();
                    if (dialog.getReturnCode() == Window.OK)
                        entry = dialog.getSelectedEntry();
                }
                
                if (entry != null && entry.getFile() == null)
                    entry = null;

                if (entry != null) {
                    IFile file = entry.getFile();
                    int linenumber = entry.getLineNumber();
                    try {
                        openEditorAtLine(file, linenumber - 1);
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
    
    private void openEditorAtLine(IFile file, int linenumber) throws PartInitException
    {
        IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
        IDE.openEditor(page, file, true);
        IEditorPart editor = page.getActiveEditor();
        if (editor instanceof SchemeEditor) {
            SchemeEditor schemeEditor = (SchemeEditor)editor;
            try {
                int lineStart = schemeEditor.getDocument().getLineOffset(linenumber);
                int lineEnd   = lineStart + schemeEditor.getDocument().getLineLength(linenumber);
                schemeEditor.setSelection(lineStart, lineEnd);
            }
            catch (BadLocationException exception) {
            }
        }
    }

    private SymbolEntry[] boostPriorities(SymbolEntry[] entries) {
        List list = Arrays.asList(entries);
        
        Collections.sort(list, new Comparator() {
            public int compare(Object o1, Object o2) {
                SymbolEntry e1 = (SymbolEntry)o1;
                SymbolEntry e2 = (SymbolEntry)o2;
                int p1 = e1.getPriority();
                int p2 = e2.getPriority();
                if (e1.getFile() != null && e1.getFile().equals(getResource()))
                    p1 += 10;
                if (e2.getFile() != null && e2.getFile().equals(getResource()))
                    p2 += 10;
                if (p1 < p2) return 1;
                if (p1 == p2) return 0;
                else return -1;
            }
        });
        
        return (SymbolEntry[]) list.toArray(new SymbolEntry[list.size()]);
    }
}