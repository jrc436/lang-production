package util.sentiment;

import java.util.Properties;

import edu.stanford.nlp.ling.CoreAnnotations;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.sentiment.SentimentCoreAnnotations;
import edu.stanford.nlp.util.CoreMap;

public class SentimentLayer {
	private final StanfordCoreNLP tokenizer;
	private final StanfordCoreNLP pipeline;

	public SentimentLayer() {
		Properties pipelineProps = new Properties();
		pipelineProps.setProperty("annotators", "parse, sentiment");
		pipelineProps.setProperty("enforceRequirements", "false");
		Properties tokenizerProps = new Properties();
		tokenizerProps.setProperty("annotators", "tokenize, ssplit");
		tokenizerProps.setProperty("ssplit.eolonly", "true");

		tokenizer = new StanfordCoreNLP(tokenizerProps);
		pipeline = new StanfordCoreNLP(pipelineProps);
	}

	public Sentiment getOutput(String line) {
		line = line.trim();
		if (line.length() > 0) {
			Annotation annotation = tokenizer.process(line);
			pipeline.annotate(annotation);			
			for (CoreMap sentence : annotation.get(CoreAnnotations.SentencesAnnotation.class)) {
				String sent = sentence.get(SentimentCoreAnnotations.SentimentClass.class);
				if (sent.equals("Very negative")) {
					return Sentiment.VeryNegative;
				} 
				else if (sent.equals("Very positive")) {
					return Sentiment.VeryPositive;
				} 
				else if (sent.equals("Positive")) {
					return Sentiment.Positive;
				} 
				else if (sent.equals("Neutral")) {
					return Sentiment.Neutral;
				} 
				else if (sent.equals("Negative")) {
					return Sentiment.Negative;
				}
				else {
					System.err.println("Sentiment was: "+sent);
				}
			}
		}
		throw new UnsupportedOperationException("FOr whatever reason, the sentiment was something else! See above...");
	}

	enum Sentiment {
		Positive, VeryPositive, Negative, VeryNegative, Neutral;
	}

}
