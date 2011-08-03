import de.be4.classicalb.core.parser.analysis.*;
import de.be4.classicalb.core.parser.node.*;

public class LispTransformer extends DepthFirstAdapter {
  private StringBuilder sb = new StringBuilder();

@Override
	public void caseAIntegerExpression(final AIntegerExpression node) {
		sb.append("(b.core/AIntegerExpression ");
		sb.append(node.getLiteral().getText());
		sb.append(")");
	}

	@Override
	public void defaultIn(final Node n) {
		sb.append("(");
		sb.append("b.core/");
		sb.append(n.getClass().getSimpleName());
	}

	@Override
	public void defaultOut(final Node n) {
		sb.append(")");
	}


  public String getAst() {
	return sb.toString();
  }

}