/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.parser;

import java.util.*;

import org.eclipse.jface.text.*;

import org.schemeway.plugins.schemescript.editor.*;

public final class BackwardTokenIterator implements ISchemeTokenIterator {
    private IDocument mDocument;
    private SchemeScanner mScanner;
    private ITypedRegion mCurrentPartition;
    private int mPosition;
    private int mStart;
    private int mLineStart;
    private Stack mTokenBuffer;
    private String mType;

    public BackwardTokenIterator(IDocument document) {
        mDocument = document;
        mScanner = new SchemeScanner();
        mTokenBuffer = new Stack();
    }

    public SchemeToken nextToken() {
        return nextToken(true);
    }
    
    public SchemeToken nextToken(boolean ignoreWS) {
        SchemeToken result = null;

        while (result == null) {
            if (mPosition == mStart)
                fetchPreviousPartition();

            if (mCurrentPartition == null)
                result = SchemeToken.EOF;
            else {
                if (mType == SchemePartitionScanner.SCHEME_COMMENT) {
                    mPosition = mStart;
                    fetchPreviousPartition();
                    continue;
                }
                else
                    if (mType == SchemePartitionScanner.SCHEME_STRING) {
                        int length = mPosition - mStart;
                        mPosition = mStart;
                        result = SchemeToken.createString(mStart, length);
                    }
                    else {
                        if (mTokenBuffer.isEmpty()) {
                            mPosition = Math.max(mLineStart - 1, mStart);
                            if (mPosition > mStart)
                                fetchTokens();
                        }
                        else {
                            SchemeToken currentToken = (SchemeToken) mTokenBuffer.pop();
                            if (!ignoreWS || currentToken.getType() != SchemeToken.WSPACE)
                                result = currentToken;
                        }
                    }
            }
        }
        return result;
    }

    public void setPosition(int position) {
        try {
            mPosition = position;
            mCurrentPartition = mDocument.getPartition(position);
            mType = mCurrentPartition.getType();
            mStart = mCurrentPartition.getOffset();
            if (mType == SchemePartitionScanner.SCHEME_COMMENT) {
                mPosition = mStart;
            }
            else
                if (mType == SchemePartitionScanner.SCHEME_STRING) {
                    mPosition = mStart + mCurrentPartition.getLength();
                    if (mStart == position) {
                        mPosition = mStart;
                    }
                }
                else
                    fetchTokens();
        }
        catch (BadLocationException exception) {
            mCurrentPartition = null;
        }
    }

    private void fetchPreviousPartition() {
        mPosition--;
        try {
            if (mPosition >= 0) {
                mCurrentPartition = mDocument.getPartition(mPosition);
                mType = mCurrentPartition.getType();
                while (mType == SchemePartitionScanner.SCHEME_COMMENT) {
                    mPosition = mCurrentPartition.getOffset() - 1;
                    mCurrentPartition = mDocument.getPartition(mPosition);
                    mType = mCurrentPartition.getType();
                }
                mStart = mCurrentPartition.getOffset();
                mPosition = mStart + mCurrentPartition.getLength();
                if (mType != SchemePartitionScanner.SCHEME_STRING) {
                    fetchTokens();
                }
            }
            else
                mCurrentPartition = null;
        }
        catch (BadLocationException exception) {
            mCurrentPartition = null;
        }
    }

    private void fetchTokens() {
        mTokenBuffer.clear();
        try {
            IRegion lineInfo = mDocument.getLineInformationOfOffset(mPosition);
            mLineStart = Math.max(mStart, lineInfo.getOffset());

            mScanner.setRange(mDocument, mLineStart, lineInfo.getLength());
            SchemeToken token = mScanner.nextToken();
            while (token != SchemeToken.EOF && token.getOffset() < mPosition) {
                mTokenBuffer.push(token);
                token = mScanner.nextToken();
            }
        }
        catch (BadLocationException exception) {
        }
    }
}