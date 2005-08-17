/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING
 */
package org.schemeway.plugins.schemescript.editor;

import org.eclipse.core.filebuffers.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.rules.*;

public class SchemeDocumentSetupParticipant implements IDocumentSetupParticipant {

    public SchemeDocumentSetupParticipant() {
    }

    public void setup(IDocument document) {
        String[] partitions = new String[]
        {
            SchemePartitionScanner.SCHEME_STRING, SchemePartitionScanner.SCHEME_COMMENT
        };

        IDocumentPartitioner partitioner = new FastPartitioner(new SchemePartitionScanner(), partitions);
        partitioner.connect(document);
        document.setDocumentPartitioner(partitioner);
    }
}