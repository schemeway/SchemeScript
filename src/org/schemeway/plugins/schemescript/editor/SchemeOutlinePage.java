/*
 * Copyright (c) 2004 Nu Echo Inc.
 * 
 * This is free software. For terms and warranty disclaimer, see ./COPYING 
 */
package org.schemeway.plugins.schemescript.editor;

import java.io.*;
import java.util.*;
import java.util.List;

import org.eclipse.core.runtime.*;
import org.eclipse.jface.text.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.views.contentoutline.*;
import org.schemeway.plugins.schemescript.*;

public class SchemeOutlinePage extends ContentOutlinePage implements ISchemeOutlinePage {
    
    private static final String MENU_DELIMITER = ";;;; --";
    private static final String SECTION_START = ";;;;   ";
    private static final String CHAPTER_START = ";;;; ";
    private static final String CHAPTER_CATEGORY = "___chapters";
    private static final String CHAPTER = "chapter";
    private static final String SECTION = "section";
    private static Image CHAPTER_IMAGE;
    private static Image SECTION_IMAGE;
    
    static {
        try {
            CHAPTER_IMAGE = new Image(Display.getDefault(), SchemeScriptPlugin.getDefault().find(new Path("icons/chapter.gif")).openStream());
            SECTION_IMAGE = new Image(Display.getDefault(), SchemeScriptPlugin.getDefault().find(new Path("icons/section.gif")).openStream());
        }
        catch (Throwable exception) {
            CHAPTER_IMAGE = null;
            SECTION_IMAGE = null;
        }
    }
    
    private static class Section {
        public String   name;
        public Position position;
        public List     children;
        public Section  parent;
        public String   type;
        
        public Section(String type, String name, Position position) {
            this.type = type;
            this.name = name;
            this.position = position;
            this.parent = null;
            this.children = new LinkedList();
        }
        
        public void addSubsection(Section section) {
            section.parent = this;
            children.add(section);
        }
        
        public Section[] getChildren() {
            return (Section[]) children.toArray(new Section[children.size()]);
        }
        
        public static Section createSection(String name, Position position) {
            return new Section(SECTION, name, position);
        }

        public static Section createChapter(String name, Position position) {
            return new Section(CHAPTER, name, position);
        }
    }
    
    private static class SectionContentProvider implements ITreeContentProvider {
        
        public Object[] getChildren(Object parentElement)
        {
            if (parentElement != null) {
                return ((Section)parentElement).getChildren();
            }
            return null;
        }
        public Object getParent(Object element)
        {
            return ((Section)element).parent;
        }
        
        public boolean hasChildren(Object element)
        {
            return ((Section)element).children.size() > 0;
        }
        
        public Object[] getElements(Object inputElement)
        {
            return getChildren(inputElement);
        }
        
        public void dispose()
        {
        }
        
        public void inputChanged(Viewer viewer, Object oldInput, Object newInput)
        {
        }
}
   
    private static class SectionLabelProvider extends LabelProvider {
        public String getText(Object element) {
            return ((Section)element).name;
        }
        
        public Image getImage(Object element)
        {
            Section section = (Section)element;
            if (section.type == SECTION) {
                return SECTION_IMAGE;
            }
            else {
                return CHAPTER_IMAGE;
            }
        }
    }
    
    private SchemeEditor mEditor;
    private IPositionUpdater mPositionUpdater = new DefaultPositionUpdater(CHAPTER_CATEGORY);
    
    public SchemeOutlinePage(SchemeEditor editor) {
        super();
        mEditor = editor;
    }
    
    public void createControl(Composite parent) {
        super.createControl(parent);
        
        TreeViewer viewer = getTreeViewer();
        viewer.setContentProvider(new SectionContentProvider());
        viewer.setLabelProvider(new SectionLabelProvider());
        viewer.addSelectionChangedListener(this);
        viewer.setInput(createTree());
        viewer.expandAll();
    }
    
    private Section createTree() {
        Section root = null;
        try {
            IDocument document = mEditor.getDocument();

            cleanPositions(document);
            document.addPositionUpdater(mPositionUpdater);
            
            document.addPositionCategory(CHAPTER_CATEGORY);
            Position position;
            
            position = new Position(0,0);
            document.addPosition(CHAPTER_CATEGORY, position);
            root = Section.createChapter("Root", position);
            Section current = root;
            
            int offset = 0;
            int documentLength = document.getLength();
            while (offset < documentLength) {
                ITypedRegion partition = document.getPartition(offset);
                if (partition.getType() == SchemePartitionScanner.SCHEME_COMMENT) {
                    String text = document.get(partition.getOffset(), partition.getLength());
                    if (text.startsWith(SECTION_START)) {
                        position = new Position(offset + 7, text.length() - 7);
                        document.addPosition(CHAPTER_CATEGORY, position);
                        Section newSection = Section.createSection(text.substring(7), position);
                        current.addSubsection(newSection);
                    }
                    else if (text.startsWith(CHAPTER_START) && !(text.equals(MENU_DELIMITER))) {
                        position = new Position(offset + 5, text.length() - 5);
                        document.addPosition(CHAPTER_CATEGORY, position);
                        Section newSection = Section.createChapter(text.substring(5), position);
                        if (current.parent != null) {
                            current = current.parent;
                        }
                        current.addSubsection(newSection);
                        current = newSection;
                    }
                }
                offset += partition.getLength();
            }
        }
        catch (BadLocationException exception) {
        }
        catch (BadPositionCategoryException exception) {
        }
        return root;
    }
    
    private void cleanPositions(IDocument document)
    {
        try {
            document.removePositionCategory(CHAPTER_CATEGORY);
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
            Section section = (Section) ((IStructuredSelection) selection).getFirstElement();
            mEditor.setSelection(section.position.offset, section.position.offset + section.position.length);
        }
    }
}
