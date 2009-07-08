/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.editor.outline;

import org.eclipse.jface.text.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.views.contentoutline.*;
import org.schemeway.plugins.schemescript.editor.*;

public class SchemeOutlinePage extends ContentOutlinePage implements ISchemeOutlinePage {
    
    
    private OutlineContentBuilder mContentBuilder = new SectionsAndDefinitionsContentBuilder();
    private SchemeEditor mEditor;
    
    private IPositionUpdater mPositionUpdater = new DefaultPositionUpdater(ContentUtilities.SECTION_CATEGORY);
    
    
    public SchemeOutlinePage(SchemeEditor editor) {
        super();
        mEditor = editor;
    }
    
    public void createControl(Composite parent) {
        super.createControl(parent);
        
        TreeViewer viewer = getTreeViewer();
        viewer.setContentProvider(new OutlineNodeContentProvider());
        viewer.setLabelProvider(new NodeLabelProvider());
        OutlineNode tree = createTree();
        viewer.setInput(tree);
        Tree treeControl = (Tree) viewer.getControl();
        if (tree.size() > 0) {
        	TreeItem item = treeControl.getItem(0);
        	treeControl.setSelection(new TreeItem[] { item });
        }
        viewer.expandAll();
    }
    
    private OutlineNode createTree() {
        OutlineNode root = null;
        try {
            IDocument document = mEditor.getDocument();

            cleanPositions(document);
            document.addPositionUpdater(mPositionUpdater);
            document.addPositionCategory(ContentUtilities.SECTION_CATEGORY);
            
            root = createRoot(document);

            OutlineNode[] nodes = mContentBuilder.buildNodes(mEditor);
            ContentUtilities.populateTree(root, nodes);
        }
        catch (BadLocationException exception) {
        }
        catch (BadPositionCategoryException exception) {
        }
        return root;
    }

	private OutlineNode createRoot(IDocument document) throws BadLocationException, BadPositionCategoryException {
		OutlineNode root;
		Position position;
		position = new Position(0,0);
		document.addPosition(ContentUtilities.SECTION_CATEGORY, position);
		root = OutlineNode.createChapter("Root", position, 0);
		return root;
	}

    private void cleanPositions(IDocument document)
    {
        try {
            document.removePositionCategory(ContentUtilities.SECTION_CATEGORY);
            document.removePositionUpdater(mPositionUpdater);
        }
        catch (BadPositionCategoryException exception) {
        }
    }

    public void update() {
        TreeViewer viewer = getTreeViewer();
        viewer.getControl().setRedraw(false);
        getTreeViewer().setInput(createTree());
        viewer.expandAll();
        viewer.getControl().setRedraw(true);
    }
    
    public void selectionChanged(SelectionChangedEvent event) {
        super.selectionChanged(event);

        ISelection selection= event.getSelection();
        if (selection.isEmpty())
            mEditor.resetHighlightRange();
        else {
            OutlineNode section = (OutlineNode) ((IStructuredSelection) selection).getFirstElement();
            mEditor.setSelection(section.position.offset, section.position.offset + section.position.length);
        }
    }
}
