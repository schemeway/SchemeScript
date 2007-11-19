/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.editor;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jdt.core.*;
import org.eclipse.jdt.ui.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.IRegion;
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

			if (isTypeName(symbol)) {
				IJavaElement javaElement = findJavaElement(symbol.substring(1, symbol.length() - 1));
				if (javaElement != null) {
					return new IHyperlink[] { new JavaElementHyperlink(symbolRegion, symbol, javaElement) };
				}
			}
		}
		catch (BadLocationException e) {
		}
		return null;
	}

	private IJavaElement findJavaElement(String fullyQualifiedPath) {
		Path javaPath = new Path(fullyQualifiedPath + ".java");
		Path classPath = new Path(fullyQualifiedPath + ".class");

		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IWorkspaceRoot root = workspace.getRoot();
		IJavaModel javaModel = JavaCore.create(root);

		IJavaProject[] javaProjects;
		try {
			javaProjects = javaModel.getJavaProjects();
			for (int javaProjectIndex = 0; javaProjectIndex < javaProjects.length; javaProjectIndex++) {
				IJavaProject javaProject = javaProjects[javaProjectIndex];
				
				IJavaElement javaElement = javaProject.findElement(classPath);
				if (javaElement != null)
					return javaElement;
				javaElement = javaProject.findElement(javaPath);
				if (javaElement != null)
					return javaElement;
			}
		}
		catch (JavaModelException e) {
		}

		return null;
	}

	private boolean isTypeName(String symbol) {
		return symbol.startsWith("<") && symbol.endsWith(">");
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

	private class JavaElementHyperlink implements IHyperlink {
		private IRegion mRegion;
		private String mLabel;
		private IJavaElement mJavaElement;

		public JavaElementHyperlink(IRegion region, String label, IJavaElement javaElement) {
			super();
			mRegion = region;
			mLabel = label;
			mJavaElement = javaElement;
		}

		public IRegion getHyperlinkRegion() {
			return mRegion;
		}

		public String getHyperlinkText() {
			return mLabel;
		}

		public String getTypeLabel() {
			return null;
		}

		public void open() {
			try {
				JavaUI.openInEditor(mJavaElement);
			}
			catch (Exception e) {
			}
		}

	}
}
