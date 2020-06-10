package org.search.pretreat.core.model.response;

import java.util.List;
import java.util.Map;

public class QueryUnderstand {
    private List<Instruction> instructions;
    private boolean isFoodQuery;
    private boolean isRetailQuery;
    private boolean isVagueQuery;
    private boolean isProhibitedQuery;
    private boolean isExemptFromFilter;
    private Map<Integer, Double> categoryIntent;
    private String stickToTopShopName;
    private Map<String, String> BDILabelMap;
    private List<String> relevantQueries;

    public List<Instruction> getInstructions() {
        return instructions;
    }

    public boolean isFoodQuery() {
        return isFoodQuery;
    }

    public boolean isRetailQuery() {
        return isRetailQuery;
    }

    public boolean isVagueQuery() {
        return isVagueQuery;
    }

    public boolean isProhibitedQuery() {
        return isProhibitedQuery;
    }

    public void setProhibitedQuery(boolean prohibitedQuery) {
        isProhibitedQuery = prohibitedQuery;
    }

    public boolean isExemptFromFilter() {
        return isExemptFromFilter;
    }

    public void setExemptFromFilter(boolean exemptFromFilter) {
        isExemptFromFilter = exemptFromFilter;
    }

    public Map<Integer, Double> getCategoryIntent() {
        return categoryIntent;
    }

    public String getStickToTopShopName() {
        return stickToTopShopName;
    }

    public Map<String, String> getBDILabelMap() {
        return BDILabelMap;
    }

    public void setInstructions(List<Instruction> instructions) {
        this.instructions = instructions;
    }

    public void setFoodQuery(boolean foodQuery) {
        isFoodQuery = foodQuery;
    }

    public void setRetailQuery(boolean retailQuery) {
        isRetailQuery = retailQuery;
    }

    public void setVagueQuery(boolean vagueQuery) {
        isVagueQuery = vagueQuery;
    }

    public void setCategoryIntent(Map<Integer, Double> categoryIntent) {
        this.categoryIntent = categoryIntent;
    }

    public void setStickToTopShopName(String stickToTopShopName) {
        this.stickToTopShopName = stickToTopShopName;
    }

    public void setBDILabelMap(Map<String, String> BDILabelMap) {
        this.BDILabelMap = BDILabelMap;
    }

    public List<String> getRelevantQueries() {
        return relevantQueries;
    }

    public void setRelevantQueries(List<String> relevantQueries) {
        this.relevantQueries = relevantQueries;
    }

    @Override
    public String toString() {
        return instructions + "\tisFoodQuery=" + isFoodQuery + "\tisRetailQuery=" + isRetailQuery +
                "\tisVagueQuery=" + isVagueQuery + "\tcategoryIntent=" + categoryIntent +
                "\tstickToTopShopName=" + stickToTopShopName + "\trelevantQueries=" + relevantQueries;
    }
}
