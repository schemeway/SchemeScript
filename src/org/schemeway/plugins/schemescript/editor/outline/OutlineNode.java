/*
 * Copyright (c) 2004-2006 SchemeWay Project. All rights reserved.
 */
package org.schemeway.plugins.schemescript.editor.outline;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.jface.text.Position;

public class OutlineNode {
	public static final String CHAPTER = "chapter";
	public static final String SECTION = "section";
	public static final String DEFINITION = "definition";

	public String name;
	public Position position;
	public List children;
	public OutlineNode parent;
	public String type;
	public int level;

	public OutlineNode(String type, String name, Position position, int level) {
		this.type = type;
		this.name = name;
		this.position = position;
		this.parent = null;
		this.level = level;
		this.children = new LinkedList();
	}

	public void addSubsection(OutlineNode section) {
		section.parent = this;
		for (int i = 0; i < children.size(); i++) {
			OutlineNode child = (OutlineNode) children.get(i);
			if (child.position.offset > section.position.offset) {
				children.add(i, section);
				return;
			}
		}
		children.add(section);
	}

	public OutlineNode[] getChildren() {
		return (OutlineNode[]) children.toArray(new OutlineNode[children.size()]);
	}

	public int size() {
		return children.size();
	}

	public static OutlineNode createSection(String name, Position position, int level) {
		return new OutlineNode(SECTION, name, position, level);
	}

	public static OutlineNode createChapter(String name, Position position, int level) {
		return new OutlineNode(CHAPTER, name, position, level);
	}

	public static OutlineNode createDefinition(String name, Position position, int level) {
		return new OutlineNode(DEFINITION, name, position, level);
	}

	public Object clone() {
		return new OutlineNode(type, name, position, level);
	}
}