/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.editor.outline;

import java.util.*;

import org.eclipse.jface.text.*;
import org.eclipse.ui.*;
import org.schemeway.plugins.schemescript.dictionary.*;
import org.schemeway.plugins.schemescript.editor.*;

/**
 * @author SchemeWay Project.
 *
 */
public final class ContentUtilities {
	private static final String MENU_DELIMITER = ";;;; --";
	private static final String CHAPTER_PREFIX = ";;;; ";
	
	public static final String SECTION_CATEGORY = "___chapters";
	public static final OutlineNodePositionComparator NODE_COMPARATOR = new OutlineNodePositionComparator();

	private ContentUtilities() {
		// prevent instantiation
	}

	public static String createNodeName(SymbolEntry entry) {
		return entry.getName() + " - " + entry.getCategory();
	}

	public static OutlineNode createNodeForEntry(IDocument document, SymbolEntry entry) throws BadLocationException {
		int offset = document.getLineOffset(entry.getLineNumber() - 1);
		Position position = new Position(offset);
		document.addPosition(position);
		OutlineNode node = OutlineNode.createDefinition(createNodeName(entry), position, 0);
		return node;
	}

	public static OutlineNode createNodesForEntryRecursively(IDocument document, SymbolEntry entry, List topNodes, HashMap entriesProcessed) throws BadLocationException {
	    OutlineNode node = (OutlineNode) entriesProcessed.get(entry);
	    if (node == null) {
	        node = createNodeForEntry(document, entry);
	        entriesProcessed.put(entry , node);
	        
	    	SymbolEntry parent = entry.getParent();
	    	if (parent == null) {
	    		topNodes.add(node);
	    	}
	    	else {
	    		OutlineNode parentNode = createNodesForEntryRecursively(document, parent, topNodes, entriesProcessed);
	    		parentNode.addSubsection(node);
	    	}
	    }
		return node;
	}

	public static void addDefinitions(SchemeEditor editor, List topNodes, boolean recurse) throws BadLocationException {
	    IDocument document = editor.getDocument();
	    IEditorInput input = editor.getEditorInput();
	    	
	    HashMap entriesProcessed = new HashMap();
	
	    if (input instanceof IFileEditorInput) {
	        IFileEditorInput editorInput = (IFileEditorInput) input;
	        List entries = editor.getSymbolDictionary().findSymbolForResource(editorInput.getFile());
	        for (int index = 0; index < entries.size(); index++) {
	            SymbolEntry entry = (SymbolEntry) entries.get(index);
	            
	            if ("java-member".equals(entry.getCategory()))
	            	continue;
	            
	            if (recurse) {
	            	createNodesForEntryRecursively(document, entry, topNodes, entriesProcessed);
	            }
	            else {
	            	topNodes.add(createNodeForEntry(document, entry));
	            }
	        }
	    }
	}

	public static void addSections(IDocument document, List nodes) throws BadLocationException, BadPositionCategoryException
	{
	    Position position;
	    int offset = 0;
	    int documentLength = document.getLength();
	    while (offset < documentLength) {
	        ITypedRegion partition = SchemeTextUtilities.getPartition(document, offset);
	        if (partition.getType() == SchemePartitionScanner.SCHEME_COMMENT) {
	            String text = document.get(partition.getOffset(), partition.getLength());
	            text = text.trim();
	            if (text.startsWith(CHAPTER_PREFIX) && !(text.equals(MENU_DELIMITER))) {
	            	// find sectioning level
	            	int titleOffset = 5;
	            	int textLength = text.length();
	            	int level = 1;
	            	while (titleOffset < textLength && text.charAt(titleOffset) == '*') {
	            		titleOffset++;
	            		level++;
	            	}
	            	// skip whitespaces
	                while (titleOffset < textLength && Character.isWhitespace(text.charAt(titleOffset))) {
	                	titleOffset ++;
	                }
	                position = new Position(offset + titleOffset, text.length() - (4 + level));
	                document.addPosition(SECTION_CATEGORY, position);
	                
	                if (level == 1) {
	                	OutlineNode newSection = OutlineNode.createChapter(text.substring(titleOffset), position, level);
	                	nodes.add(newSection);
	                }
	                else {
	                	OutlineNode newSection = OutlineNode.createSection(text.substring(titleOffset), position, level);
	                	nodes.add(newSection);
	                }
	            }
	        }
	        offset += partition.getLength();
	    }
	}

	/**
	 * @param root
	 * @param nodes
	 */
	public static void populateTree(OutlineNode root, OutlineNode[] nodes)
	{
	    OutlineNode current = root;
	    
	    for (int index = 0; index < nodes.length; index++) {
			OutlineNode node = nodes[index];
			
			if (node.type == OutlineNode.CHAPTER || node.type == OutlineNode.SECTION) {
				while (node.level <= current.level) {
					current = current.parent;
				}
				current.addSubsection(node);
				current = node;
			}
			else {
				current.addSubsection(node);
			}
		}
	}
}
