/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.parser;

import org.eclipse.jface.text.*;

import org.schemeway.plugins.schemescript.editor.*;

public class ForwardTokenIterator implements ISchemeTokenIterator {
    private IDocument mDocument;
    private SchemeScanner mScanner;
    private ITypedRegion mCurrentPartition;
    private int mPosition;
    private int mEnd;

    public ForwardTokenIterator(IDocument document) {
        mDocument = document;
        mScanner = new SchemeScanner();
    }

    public SchemeToken nextToken() {
        return nextToken(true);
    }
    
    public SchemeToken nextToken(boolean ignoreWS) {
        SchemeToken result = null;

        while (result == null) {
            if (mPosition == mEnd)
                fetchNextPartition();

            if (mCurrentPartition == null)
                result = SchemeToken.EOF;
            else {
                if (mCurrentPartition.getType() == SchemePartitionScanner.SCHEME_STRING) {
                    int length = mEnd - mPosition;
                    int start = mPosition;
                    mPosition = mEnd;
                    return SchemeToken.createString(start, length);
                }
                else
                    if (mCurrentPartition.getType() == SchemePartitionScanner.SCHEME_COMMENT) {
                        fetchNextPartition();
                        continue;
                    }
                    else {
                        SchemeToken token = mScanner.nextToken();
                        mPosition = mPosition + token.getLength();
                        if (!ignoreWS || token.getType() != SchemeToken.WSPACE) {
                            result = token;
                        }
                    }
            }
        }
        return result;
    }

    public void setPosition(int position) {
        try {
            if (position < mDocument.getLength()) {
                mCurrentPartition = mDocument.getPartition(position);
                if (mCurrentPartition.getType() == SchemePartitionScanner.SCHEME_STRING) {
                    mPosition = mCurrentPartition.getOffset();
                    mEnd = mPosition + mCurrentPartition.getLength();
                }
                else
                    if (mCurrentPartition.getType() == SchemePartitionScanner.SCHEME_COMMENT) {
                        mPosition = mCurrentPartition.getOffset();
                        mEnd = mPosition + mCurrentPartition.getLength();
                    }
                    else {
                        mPosition = position;
                        mEnd = mCurrentPartition.getOffset() + mCurrentPartition.getLength();
                        mScanner.setRange(mDocument,
                                          position,
                                          (mCurrentPartition.getOffset() + mCurrentPartition.getLength()) - mPosition);
                    }
            }
            else {
                mPosition = mEnd = position;
                mCurrentPartition = null;
            }
        }
        catch (BadLocationException exception) {
            mCurrentPartition = null;
        }
    }

    private void fetchNextPartition() {
        try {
            // skip over comment partitions...
            if (mPosition < mDocument.getLength()) {
                mCurrentPartition = mDocument.getPartition(mPosition);
                while (mCurrentPartition.getType() == SchemePartitionScanner.SCHEME_COMMENT) {
                    mPosition = mCurrentPartition.getOffset() + mCurrentPartition.getLength();
                    mCurrentPartition = mDocument.getPartition(mPosition);
                }
                mPosition = mCurrentPartition.getOffset();
                mEnd = mPosition + mCurrentPartition.getLength();
                if (mCurrentPartition.getType() != SchemePartitionScanner.SCHEME_STRING)
                    mScanner.setRange(mDocument, mPosition, mEnd - mPosition);
            }
            else
                mCurrentPartition = null;
        }
        catch (BadLocationException exception) {
            mCurrentPartition = null;
        }
    }
}