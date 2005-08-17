/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor;

import org.eclipse.jface.text.*;
import org.eclipse.jface.text.presentation.*;
import org.eclipse.jface.text.rules.*;
import org.eclipse.jface.util.*;
import org.eclipse.jface.util.Assert;

import org.schemeway.plugins.schemescript.*;
import org.schemeway.plugins.schemescript.indentation.*;
import org.schemeway.plugins.schemescript.parser.*;
import org.schemeway.plugins.schemescript.preferences.*;

public class SchemeTextTools {
    private SchemeDoubleClickStrategy mDoubleClickStrategy;
    private SchemeDefaultScanner mStringScanner;
    private SchemeDefaultScanner mCommentScanner;
    private SchemeColoringScanner mColoringScanner;
    private ColorManager mColorManager;
    private SchemeIndentationManager mIndentationManager;
    private KeywordManager mKeywordManager;

    private PropertyChangeEvent mLastEvent;

    public SchemeTextTools(ColorManager colorManager) {
        Assert.isNotNull(colorManager);
        this.mColorManager = colorManager;
        this.mLastEvent = null;
    }

    public ColorManager getColorManager() {
        return mColorManager;
    }

    public SchemeIndentationManager getIndentationManager() {
        if (mIndentationManager == null) {
            mIndentationManager = new SchemeIndentationManager();
            mIndentationManager.updateSchemes();
        }
        return mIndentationManager;
    }

    public String[] getContentTypes() {
        return new String[] {
                                IDocument.DEFAULT_CONTENT_TYPE,
                                SchemePartitionScanner.SCHEME_COMMENT,
                                SchemePartitionScanner.SCHEME_STRING
        };
    }

    public ITextDoubleClickStrategy getDoubleClickStrategy(String contentType) {
        if (mDoubleClickStrategy == null) {
            mDoubleClickStrategy = new SchemeDoubleClickStrategy();
        }
        return mDoubleClickStrategy;
    }

    public IAutoEditStrategy[] getAutoEditStrategies(String contentType) {
        if (contentType == IDocument.DEFAULT_CONTENT_TYPE 
                || contentType == SchemePartitionScanner.SCHEME_COMMENT) {
            return new IAutoEditStrategy[] { new SchemeIndentationStrategy(getIndentationManager()) };
        }
        return new IAutoEditStrategy[] { };
    }

    public SchemeColoringScanner getSchemeColoringScanner() {
        if (mColoringScanner == null) {
            mColoringScanner = new SchemeColoringScanner(mColorManager, getKeywordManager());
        }
        return mColoringScanner;
    }

    public KeywordManager getKeywordManager() {
        if (mKeywordManager == null) {
            mKeywordManager = new KeywordManager();

            PreferenceUtil.updateKeywordManager(SchemeScriptPlugin.getDefault().getPreferenceStore(), mKeywordManager);
        }
        return mKeywordManager;
    }

    public SchemeDefaultScanner getSchemeStringScanner() {
        if (mStringScanner == null) {
            mStringScanner = new SchemeDefaultScanner(mColorManager, ColorPreferences.STRING_COLOR);
        }
        return mStringScanner;
    }

    public SchemeDefaultScanner getSchemeCommentScanner() {
        if (mCommentScanner == null) {
            mCommentScanner = new SchemeDefaultScanner(mColorManager, ColorPreferences.COMMENT_COLOR);
        }
        return mCommentScanner;
    }

    public IPresentationReconciler getPresentationReconciler() {
        PresentationReconciler reconciler = new PresentationReconciler();

        DefaultDamagerRepairer dr = new DefaultDamagerRepairer(getSchemeColoringScanner());
        reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
        reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);

        dr = new DefaultDamagerRepairer(getSchemeStringScanner());
        reconciler.setDamager(dr, SchemePartitionScanner.SCHEME_STRING);
        reconciler.setRepairer(dr, SchemePartitionScanner.SCHEME_STRING);

        dr = new DefaultDamagerRepairer(getSchemeCommentScanner());
        reconciler.setDamager(dr, SchemePartitionScanner.SCHEME_COMMENT);
        reconciler.setRepairer(dr, SchemePartitionScanner.SCHEME_COMMENT);

        return reconciler;
    }

    public void updateColors(PropertyChangeEvent event) {
        // make sure we don't update if it's called more than once for the same
        // event
        if (event != mLastEvent) {
            mLastEvent = event;
            getSchemeColoringScanner().updateColors();
            getSchemeStringScanner().updateColors();
            getSchemeCommentScanner().updateColors();
        }
    }

    public void updateIndentationSchemes() {
        getIndentationManager().updateSchemes();
    }
}