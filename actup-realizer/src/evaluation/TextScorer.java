package evaluation;

/**
 * Anything that evaluates realizations
 * @author jrc
 *
 */
public interface TextScorer {
	public double score(String trial, String gold);
}
