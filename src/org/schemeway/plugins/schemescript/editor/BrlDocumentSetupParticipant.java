/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.editor;

import org.eclipse.core.filebuffers.IDocumentSetupParticipant;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.DefaultPartitioner;

/**
 * @author SchemeWay Project.
 *
 */
public class BrlDocumentSetupParticipant implements IDocumentSetupParticipant {

	public BrlDocumentSetupParticipant() {
	}
	
    public void setup(IDocument document) {
        String[] partitions = new String[]
        {
            SchemePartitionScanner.SCHEME_STRING, SchemePartitionScanner.SCHEME_COMMENT, SchemePartitionScanner.SCHEME_HERESTRING
        };

        IDocumentPartitioner partitioner = new DefaultPartitioner(new BrlPartitionScanner(), partitions);
        partitioner.connect(document);
        document.setDocumentPartitioner(partitioner);
    }
}
