task :ruby => ['ruby:lexer','ruby:parser','ruby:test']
task :haskell => ['haskell:lexer','haskell:parser','haskell:test']

task :clean => ['ruby:clean','haskell:clean']

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
		sh 'rm -f first-order-logic a.out *.{hs,hi,info,o} **/*.{hi,o}'
	end

	desc 'Test the generated parser against the sample program'
	task :test do
		sh 'ghc -o first-order-logic first-order-logic{.x,}.hs'
		sh 'chmod u+x first-order-logic'
		sh 'cat simple-grammar-sample.fol | ./first-order-logic'
	end

end
