/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript;

import java.io.*;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.text.*;
import org.eclipse.ui.*;
import org.eclipse.ui.ide.*;
import org.schemeway.plugins.schemescript.editor.*;

public final class SchemeScriptTools {

    private SchemeScriptTools() {
        // prevent instantiation
    }

    public static IFile findFile(String filename, String baseDirectory) {
        if (filename != null && !filename.equals("")) {
            File file = new File(filename);
            if (!file.isAbsolute()) {
                if (baseDirectory == null)
                    return null;
                File directory = new File(baseDirectory);
                try {
                    filename = (new File(directory, filename).getCanonicalPath());
                }
                catch (IOException exception) {
                    return null;
                }
            }
            IWorkspace ws = ResourcesPlugin.getWorkspace();
            IWorkspaceRoot root = ws.getRoot();
            return root.getFileForLocation(new Path(filename));
        }
        return null;
    }

    public static void openEditor(IFile file, int linenumber) {
        try {
            IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
            IEditorPart editor;
            editor = IDE.openEditor(page, file, true);
            if (editor == null) {
                return;
            }

            if (editor instanceof SchemeEditor) {
                SchemeEditor schemeEditor = (SchemeEditor) editor;
                try {
                    int lineStart = schemeEditor.getDocument().getLineOffset(linenumber);
                    int lineEnd = lineStart + schemeEditor.getDocument().getLineLength(linenumber);
                    schemeEditor.setSelection(lineStart, lineEnd);
                }
                catch (BadLocationException exception) {
                }
            }
        }
        catch (PartInitException e) {
            SchemeScriptPlugin.logException("Unable to open editor", e);
        }
    }
}