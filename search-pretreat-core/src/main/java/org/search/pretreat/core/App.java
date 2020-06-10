package org.search.pretreat.core;

import org.search.pretreat.core.commander.FoodCommander;

/**
 * Hello world!
 *
 */
public class App {
	public static void main(String[] args) {
		boolean isFood = FoodCommander.isFoodQuery("汉堡");
		System.out.println("Hello World!" + isFood);
	}
}
