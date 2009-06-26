/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.dictionary;

import org.eclipse.jface.layout.*;
import org.eclipse.jface.resource.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.search.ui.*;
import org.eclipse.swt.*;
import org.eclipse.swt.events.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.layout.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.*;
import org.eclipse.ui.texteditor.*;

/**
 * @author SchemeWay Project.
 * 
 */
public class SymbolSearchPage implements ISearchPage {

	private ISearchPageContainer mContainer;
	private Control mMainControl;

	private Text mSymbolText;

	public boolean performAction() {
		NewSearchUI.runQueryInBackground(createQuery());
		return true;
	}

	private SymbolSearchQuery createQuery() {
		return new SymbolSearchQuery(mSymbolText.getText());
	}

	public void setContainer(ISearchPageContainer container) {
		mContainer = container;
	}

	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		mMainControl = composite;
		composite.setLayout(new GridLayout(1, true));

		Label label = new Label(composite, SWT.NONE);
		label.setText("Symbol to search:");
		GridDataFactory.fillDefaults().applyTo(label);

		mSymbolText = new Text(composite, SWT.BORDER);
		GridDataFactory.fillDefaults().align(SWT.FILL, SWT.TOP).grab(true, false).applyTo(mSymbolText);
		mSymbolText.setText(getDefaultSearchText());
		mSymbolText.setSelection(0, mSymbolText.getText().length());

		mSymbolText.addKeyListener(new KeyAdapter() {
			public void keyReleased(KeyEvent e) {
				if (mSymbolText.getText().trim().equals("")) {
					mContainer.setPerformActionEnabled(false);
				}
				else {
					mContainer.setPerformActionEnabled(true);
				}
			}
		});
	}

	private String getDefaultSearchText() {
		IEditorPart activeEditor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()
				.getActiveEditor();
		if (activeEditor instanceof AbstractDecoratedTextEditor) {
			AbstractDecoratedTextEditor editor = (AbstractDecoratedTextEditor) activeEditor;
			ISelection selection = editor.getSelectionProvider().getSelection();
			if (selection instanceof TextSelection) {
				TextSelection textSelection = (TextSelection) selection;
				return textSelection.getText();
			}
		}

		return "";
	}

	public void dispose() {
		mMainControl = null;
		mContainer = null;
	}

	public Control getControl() {
		return mMainControl;
	}

	public String getDescription() {
		return null;
	}

	public String getErrorMessage() {
		return null;
	}

	public Image getImage() {
		return null;
	}

	public String getMessage() {
		return null;
	}

	public String getTitle() {
		return "Scheme Search";
	}

	public void performHelp() {
	}

	public void setDescription(String description) {
	}

	public void setImageDescriptor(ImageDescriptor image) {
	}

	public void setTitle(String title) {
	}

	public void setVisible(boolean visible) {
		if (visible) {
			mSymbolText.setFocus();
		}
	}

}
