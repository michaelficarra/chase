task :ruby => ['ruby:lexer','ruby:parser','ruby:test']
task :haskell => ['haskell:lexer','haskell:parser','haskell:test']
task :paper => ['paper:pdf','paper:view']

task :clean => ['ruby:clean','haskell:clean','paper:clean']

namespace 'ruby' do

	desc 'Generate lexer with rexical'
	task :lexer do
		sh 'rex *.rex --stub'
	end

	desc 'Generate parser with racc'
	task :parser do
		sh 'racc -SEv -e "/usr/bin/env ruby" *.racc'
	end

	desc 'Clean up generated files and files output during debugging'
	task :clean do
		sh 'rm -f **/*.{output,rex.rb,tab.rb}'
	end

	desc 'Test the generated parser against the sample program'
	task :test do
		sh 'ruby -Ku *.tab.rb simple-grammar-sample-unicode.fol'
	end

end


namespace 'haskell' do

	desc 'Generate lexer with alex'
	task :lexer do
		sh 'alex *.x.x'
	end

	desc 'Generate parser with happy'
	task :parser do
		sh 'happy -i *.y'
	end

	desc 'Clean up generated files and files output during debugging'
	task :clean do
		sh 'rm -rf first-order-logic{,.x.hs,.hs} a.out *.{hi,info,o} models/*'
	end

	desc 'Test the generated parser against the sample program'
	task :test do
		sh 'ghc -o first-order-logic first-order-logic{.x,}.hs helpers.hs chase.hs main.hs'
		sh 'chmod u+x first-order-logic'
		sh 'cat theories/trichotomy | ./first-order-logic'
	end

end


namespace 'paper' do

	desc 'Generate PDF from latex document'
	task :pdf do
		Dir.chdir 'paper' do
			sh 'pdflatex paper.tex'
		end
	end

	desc 'View the generated pdf in the default pdf viewer'
	task :view do
		sh 'xdg-open paper/paper.pdf'
	end

	desc 'Clean up unnecessary generated files'
	task :clean do
		sh 'rm -f *.log paper/*.{aux,toc,lof,lot,log,out}'
	end

end
