/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.editor;

import org.eclipse.core.resources.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.hyperlink.*;
import org.eclipse.ui.*;
import org.eclipse.ui.part.*;
import org.schemeway.plugins.schemescript.dictionary.*;

/**
 * @author SchemeWay Project.
 * 
 */
public class SymbolHyperlinkDetector implements IHyperlinkDetector {
	private SchemeEditor mEditor;

	public SymbolHyperlinkDetector(SchemeEditor editor) {
		super();
		mEditor = editor;
	}

	public IHyperlink[] detectHyperlinks(ITextViewer textViewer, IRegion region, boolean canShowMultipleHyperlinks) {

		try {
			IDocument document = mEditor.getDocument();
			IRegion symbolRegion = SchemeTextUtilities.findSymbolRegionAroundPoint(document, region.getOffset());
			if (symbolRegion == null) {
				return null;
			}

			String symbol = document.get(symbolRegion.getOffset(), symbolRegion.getLength());

			SymbolEntry[] entries = DictionaryUtils.findUserDefinitions(symbol);
			if (entries.length > 0) {
				return new IHyperlink[] { new SymbolHyperlink(symbolRegion, symbol, entries) };
			}
		}
		catch (BadLocationException e) {
		}
		return null;
	}

	private class SymbolHyperlink implements IHyperlink {
		private IRegion mRegion;
		private String mSymbol;
		private SymbolEntry[] mEntries;

		public SymbolHyperlink(IRegion region, String symbol, SymbolEntry[] entries) {
			super();
			mRegion = region;
			mSymbol = symbol;
			mEntries = entries;
		}

		public IRegion getHyperlinkRegion() {
			return mRegion;
		}

		public String getHyperlinkText() {
			return mSymbol;
		}

		public String getTypeLabel() {
			return null;
		}

		public void open() {
			SchemeTextUtilities.openOrSelectEntry(mEntries, getResource());
		}

		private IResource getResource() {
			IEditorInput input = mEditor.getEditorInput();
			if (input instanceof FileEditorInput) {
				return ((FileEditorInput) input).getFile();
			}
			return null;
		}

	}
}
