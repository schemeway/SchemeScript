/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.contentassist.*;
import org.eclipse.jface.text.presentation.*;
import org.eclipse.jface.text.source.*;

public class SchemeConfiguration extends SourceViewerConfiguration {
    private SchemeTextTools mTextTools;
    private IUndoManager mUndoManager;

    public SchemeConfiguration(SchemeTextTools textTools) {
        Assert.isNotNull(textTools);
        this.mTextTools = textTools;
    }

    public SchemeTextTools getTextTools() {
        return mTextTools;
    }

    public String[] getConfiguredContentTypes(ISourceViewer sourceViewer) {
        return mTextTools.getContentTypes();
    }

    public ITextDoubleClickStrategy getDoubleClickStrategy(ISourceViewer sourceViewer, String contentType) {
        return mTextTools.getDoubleClickStrategy(contentType);
    }

    public IAutoIndentStrategy getAutoIndentStrategy(ISourceViewer sourceViewer, String contentType) {
        return mTextTools.getAutoIndentStrategy(contentType);
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
        assistant.setContentAssistProcessor(new SchemeContentAssistProcessor(), IDocument.DEFAULT_CONTENT_TYPE);
        assistant.enableAutoActivation(true);
        assistant.setAutoActivationDelay(200);
        assistant.enableAutoInsert(true);
        assistant.enablePrefixCompletion(true);
        assistant.setProposalPopupOrientation(IContentAssistant.PROPOSAL_OVERLAY);
        assistant.setContextInformationPopupOrientation(IContentAssistant.CONTEXT_INFO_BELOW);
        
        return assistant;
    }

}