package org.search.pretreat.core.model.response;


import java.util.Set;

public class Instruction {
	
	private INSTRUCTION_TYPE type;
    private String value;
    private KEYWORD_RESTRICTION restriction;
	private String restriction2;	// 二级品类筛选，"-"开头为过滤，null为没有限制
	private boolean isAtomicWord;	// query必须被连续命中
	private boolean isRedPacket;
	private Set<String> evilPatterns;
	private Set<String> irrelevantPatterns;
	private CategoryIntent categoryIntent;
	private String infoString;		// 可以临时存些乱七八糟的东西
    
	public INSTRUCTION_TYPE getType() {
		return type;
	}
	public void setType(INSTRUCTION_TYPE type) {
		this.type = type;
	}
	public String getValue() {
		return value;
	}
	public void setValue(String value) {
		this.value = value;
	}
	public KEYWORD_RESTRICTION getRestriction() {
		return restriction;
	}
	public void setRestriction(KEYWORD_RESTRICTION restriction) {
		this.restriction = restriction;
	}
	public String getRestriction2() {
		return restriction2;
	}
	public void setRestriction2(String restriction2) {
		this.restriction2 = restriction2;
	}
	public Set<String> getEvilPatterns() { return evilPatterns; }
	public boolean isAtomicWord() { return isAtomicWord; }
	public void setAtomicWord(boolean atomicWord) { isAtomicWord = atomicWord; }
	public void setEvilPatterns(Set<String> evilPatterns) { this.evilPatterns = evilPatterns; }

	public Set<String> getIrrelevantPatterns() {
		return irrelevantPatterns;
	}
	public void setIrrelevantPatterns(Set<String> irrelevantPatterns) {
		this.irrelevantPatterns = irrelevantPatterns;
	}

	public CategoryIntent getCategoryIntent() {
		return categoryIntent;
	}

	public void setCategoryIntent(CategoryIntent categoryIntent) {
		this.categoryIntent = categoryIntent;
	}

	public boolean isRedPacket() { return isRedPacket; }
	public void setRedPacket(boolean redPacket) { isRedPacket = redPacket; }

	public String getInfoString() { return infoString; }
	public void setInfoString(String infoString) { this.infoString = infoString; }

	@Override

	public String toString() {
		return "Instruction [type=" + type + ", value=" + value + ", restriction=" + restriction + ", restriction2="
				+ restriction2 + ", isRedPacket=" + isRedPacket + ", evilPatterns=" + evilPatterns + ", isAtomicWord="
				+ isAtomicWord + ", irrelevantPatterns=" + irrelevantPatterns + "]";
	}
    
}

