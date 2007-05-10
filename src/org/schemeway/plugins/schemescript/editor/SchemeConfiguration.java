/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.contentassist.*;
import org.eclipse.jface.text.hyperlink.*;
import org.eclipse.jface.text.presentation.*;
import org.eclipse.jface.text.source.*;

public class SchemeConfiguration extends SourceViewerConfiguration {
    private SchemeTextTools mTextTools;
    private IUndoManager mUndoManager;
    private SchemeEditor mEditor;

    public SchemeConfiguration(SchemeTextTools textTools, SchemeEditor editor) {
        Assert.isNotNull(textTools);
        this.mTextTools = textTools;
        this.mEditor = editor;
    }

    public SchemeEditor getEditor() {
        return mEditor;
    }
    
    public SchemeTextTools getTextTools() {
        return mTextTools;
    }

    public String[] getConfiguredContentTypes(ISourceViewer sourceViewer) {
        return mTextTools.getContentTypes();
    }
    
    public String getConfiguredDocumentPartitioning(ISourceViewer sourceViewer) {
    	return SchemeDocumentSetupParticipant.SCHEME_PARTITIONING;
    }

    public ITextDoubleClickStrategy getDoubleClickStrategy(ISourceViewer sourceViewer, String contentType) {
        return mTextTools.getDoubleClickStrategy(contentType);
    }

    public IAutoEditStrategy[] getAutoEditStrategies(ISourceViewer sourceViewer, String contentType) {
        return mTextTools.getAutoEditStrategies(contentType);
    }

    public IUndoManager getUndoManager(ISourceViewer sourceViewer) {
        if (mUndoManager == null) {
            mUndoManager = super.getUndoManager(sourceViewer);
        }
        return mUndoManager;
    }

    protected SchemeColoringScanner getSchemeScanner() {
        return mTextTools.getSchemeColoringScanner();
    }

    protected SchemeDefaultScanner getSchemeStringScanner() {
        return mTextTools.getSchemeStringScanner();
    }

    public IPresentationReconciler getPresentationReconciler(ISourceViewer sourceViewer) {
        return mTextTools.getPresentationReconciler();
    }
    
    public IContentAssistant getContentAssistant(ISourceViewer sourceViewer) {
        ContentAssistant assistant= new ContentAssistant();
        assistant.setContentAssistProcessor(new SchemeContentAssistProcessor(getEditor()), IDocument.DEFAULT_CONTENT_TYPE);
        assistant.enableAutoActivation(true);
        assistant.setAutoActivationDelay(200);
        assistant.enableAutoInsert(true);
        assistant.enablePrefixCompletion(true);
        assistant.setProposalPopupOrientation(IContentAssistant.PROPOSAL_OVERLAY);
        assistant.setContextInformationPopupOrientation(IContentAssistant.CONTEXT_INFO_BELOW);
        
        return assistant;
    }
    
    public IAnnotationHover getAnnotationHover(ISourceViewer sourceViewer) {
    	return new SchemeAnnotationHover();
    }
    
    public ITextHover getTextHover(ISourceViewer viewer, String contentType) {
        return new SchemeTextHover(getEditor());
    }
    
    public IHyperlinkDetector[] getHyperlinkDetectors(ISourceViewer sourceViewer) {
    	return new IHyperlinkDetector[] {
    			new SymbolHyperlinkDetector(mEditor)
    	};
    }

}