/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.action;

import org.eclipse.ui.*;
import org.eclipse.ui.part.*;
import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.editor.*;
import org.schemeway.plugins.schemescript.interpreter.*;

public class LoadFileAction extends SchemeAction {

	public LoadFileAction(SchemeEditor editor) {
		super(editor);

		setText("Load File in Interpreter");
		setToolTipText("Load file in interpreter");
	}

	public void run() {
		SchemeEditor editor = getSchemeEditor();
		if (editor == null)
			return;

		IEditorInput input = editor.getEditorInput();
		if (input instanceof FileEditorInput) {
			FileEditorInput fileInput = (FileEditorInput) input;
			Interpreter interp = SchemeScriptPlugin.getDefault().getInterpreter();
			interp.load(fileInput.getFile());

			try {
				PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().showView(
						"org.eclipse.ui.console.ConsoleView");
			}
			catch (PartInitException e) {
			}
		}
	}
}