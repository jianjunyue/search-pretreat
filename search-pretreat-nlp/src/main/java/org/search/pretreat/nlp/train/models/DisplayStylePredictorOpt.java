package org.search.pretreat.nlp.train.models;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap; 
import org.apache.commons.lang3.StringUtils;
import org.search.pretreat.nlp.train.util.JavaUtil;

import com.hankcs.hanlp.HanLP;

public class DisplayStylePredictorOpt {
  private static DisplayStylePredictorOpt instance = null;
  
  private final Map<String, Integer> featureMap = new TreeMap<>();
  
  private final Set<String> foodSet = new HashSet<>();
  
  private Double[] weights;
  
  private final Map<String, Integer> ngramFoodMap = new HashMap<>();
  
  private static final double LABEL_THRESHOLD = 0.5D;
  
  private static final int ngram = 3;
  
  private Set<String> foodIntentIndividualCharSet = new HashSet<>();
  
  private DisplayStylePredictorOpt() {
    InputStream in = null;
    BufferedReader reader = null;
    try {
      in = getClass().getResourceAsStream("/display_style_model.v2/model.tsv");
      reader = new BufferedReader(new InputStreamReader(in));
      List<Double> buf = new ArrayList<>();
      String line;
      while ((line = reader.readLine()) != null) {
        String[] tokens = line.split("\t");
        buf.add(Double.valueOf(tokens[1]));
      } 
      in.close();
      reader.close();
      this.weights = new Double[buf.size()];
      buf.toArray(this.weights);
      in = getClass().getResourceAsStream("/display_style_model.v2/feature.tsv");
      reader = new BufferedReader(new InputStreamReader(in));
      while ((line = reader.readLine()) != null) {
        String[] tokens = line.split("\t");
        this.featureMap.put(tokens[0].trim(), Integer.valueOf(tokens[1]));
      } 
      in.close();
      reader.close();
      in = getClass().getResourceAsStream("/display_style_misc/food.commodity.word.tsv");
      reader = new BufferedReader(new InputStreamReader(in));
      while ((line = reader.readLine()) != null) {
        if (StringUtils.isNotBlank(line))
          this.foodSet.add(line); 
      } 
      in.close();
      reader.close();
      in = getClass().getResourceAsStream("/display_style_misc/food.commodity.ngram.tsv");
      reader = new BufferedReader(new InputStreamReader(in));
      while ((line = reader.readLine()) != null) {
        String[] tokens = line.split("\t");
        if (Objects.nonNull(tokens) && tokens.length == 2 && StringUtils.isNotBlank(tokens[0]) && StringUtils.isNotBlank(tokens[1]))
          this.ngramFoodMap.put(tokens[0], Integer.valueOf(tokens[1])); 
      } 
      in = getClass().getResourceAsStream("/display_style_misc/strong_food_intent_individual_words.tsv");
      reader = new BufferedReader(new InputStreamReader(in));
      while ((line = reader.readLine()) != null)
        this.foodIntentIndividualCharSet.add(line.trim()); 
    } catch (IOException ioex) {
      ioex.printStackTrace();
    } finally {
      try {
        reader.close();
        in.close();
      } catch (IOException ex) {
        ex.printStackTrace();
      } 
    } 
  }
  
  public static DisplayStylePredictorOpt getInstance() {
    if (instance == null)
      synchronized (DisplayStylePredictorOpt.class) {
        if (instance == null)
          instance = new DisplayStylePredictorOpt(); 
      }  
    return instance;
  }
  
  public double predictDist(String regQuery, boolean withBias) {
    Map<Integer, Double> vec = JavaUtil.getVector(regQuery, 3, this.featureMap, this.foodSet, this.ngramFoodMap);
    double fx = withBias ? this.weights[this.weights.length - 1].doubleValue() : 0.0D;
    for (Map.Entry<Integer, Double> entry : vec.entrySet()) {
      Integer key = entry.getKey();
      Double value = entry.getValue();
      Double keyMultiplyValue = Double.valueOf(this.weights[key.intValue() - 1].doubleValue() * value.doubleValue());
      fx += keyMultiplyValue.doubleValue();
    } 
    double score = 1.0D / (1.0D + Math.exp(-fx));
    return score;
  }
  
  public boolean isFoodDimensionQuery(String query) {
    if (JavaUtil.checkBracket(query))
      return false; 
    String regQuery = JavaUtil.processQuery(query);
    regQuery = HanLP.convertToSimplifiedChinese(regQuery);
    if (query.length() == 1)
      return this.foodIntentIndividualCharSet.contains(query); 
    double score = predictDist(regQuery, true);
    if (score >= 0.5D)
      return Boolean.FALSE.booleanValue(); 
    return Boolean.TRUE.booleanValue();
  }
  
  public static void main(String[] args) {
    String testStr = "馄炖";
    Boolean is_food_label = Boolean.valueOf(getInstance().isFoodDimensionQuery(testStr));
    System.out.println(testStr + " is_food_label:" + is_food_label);
  }
}