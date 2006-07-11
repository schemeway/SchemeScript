/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor;

import java.util.*;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.source.*;
import org.eclipse.ui.texteditor.*;
import org.schemeway.plugins.schemescript.dictionary.*;

public class SchemeTextHover implements ITextHover {
	private SchemeEditor mEditor;

	public SchemeTextHover(SchemeEditor editor) {
		super();
		mEditor = editor;
	}

	protected SchemeEditor getEditor() {
		return mEditor;
	}

	public String getHoverInfo(ITextViewer textViewer, IRegion hoverRegion) {
		if (hoverRegion == null)
			return null;

		String[] infoMessages = findDefinitions(textViewer.getDocument(), getEditor().getSymbolDictionary(),
				hoverRegion);

		if (infoMessages == null || infoMessages.length == 0) {
			infoMessages = findAnnotations(textViewer, hoverRegion);
		}
		if (infoMessages == null || infoMessages.length == 0) {
			return null;
		}

		StringBuffer buffer = new StringBuffer();
		for (int i = 0; i < infoMessages.length; i++) {
			if (i > 0)
				buffer.append('\n');
			buffer.append(infoMessages[i]);
		}
		return buffer.toString();
	}

	private String[] findDefinitions(IDocument document, ISymbolDictionary dictionary, IRegion hoverRegion) {
		List defs = new ArrayList();

		try {
			String symbol = document.get(hoverRegion.getOffset(), hoverRegion.getLength());
			SymbolEntry[] entries = getEditor().getSymbolDictionary().findSymbol(symbol);

			for (int i = 0; i < entries.length; i++) {
				String description = entries[i].getDescription();
				if (!defs.contains(description)) {
					defs.add(description);
				}
			}
		}
		catch (BadLocationException e) {
		}

		return (String[]) defs.toArray(new String[defs.size()]);
	}

	private String[] findAnnotations(ITextViewer textViewer, IRegion hoverRegion) {
		List messages = new ArrayList();
		
		if (textViewer instanceof ISourceViewer) {
			ISourceViewer sourceViewer = (ISourceViewer) textViewer;
			IAnnotationModel model = sourceViewer.getAnnotationModel();
			Iterator iterator = model.getAnnotationIterator();
			while (iterator.hasNext()) {
				Annotation annotation = (Annotation) iterator.next();
				Position position = model.getPosition(annotation);
				if (position.offset <= hoverRegion.getOffset()
						&& hoverRegion.getOffset() <= position.offset + position.length
						&& annotation instanceof MarkerAnnotation) {
					messages.add(annotation.getText());
				}
			}
		}
		return (String[]) messages.toArray(new String[messages.size()]);
	}

	public IRegion getHoverRegion(ITextViewer textViewer, int offset) {
		try {
			IRegion result = SchemeTextUtilities.findSymbolRegionAroundPoint(textViewer.getDocument(), offset);
			if (result == null) {
				result = findAnnotationRegion(textViewer, offset);
			}

			return result;
		}
		catch (BadLocationException exception) {
			return null;
		}
	}

	private IRegion findAnnotationRegion(ITextViewer textViewer, int offset) {
		if (textViewer instanceof ISourceViewer) {
			ISourceViewer sourceViewer = (ISourceViewer) textViewer;
			IAnnotationModel model = sourceViewer.getAnnotationModel();
			Iterator iterator = model.getAnnotationIterator();
			while (iterator.hasNext()) {
				Annotation annotation = (Annotation) iterator.next();
				Position position = model.getPosition(annotation);
				if (position.offset <= offset && offset <= position.offset + position.length) {
					return new Region(position.offset, position.length);
				}
			}
		}
		return null;
	}

}
