task :ruby => ['ruby:lexer','ruby:parser','ruby:test']
task :build => [:lexer,:parser,:compile]
task :default => [:build,:test]
task :paper => ['paper:pdf','paper:clean','paper:view']


namespace 'ruby' do

	desc 'Generate lexer with rexical'
	task :lexer do
		sh 'rex chase.rex --stub'
	end

	desc 'Generate parser with racc'
	task :parser do
		sh 'racc -SEv -e "/usr/bin/env ruby" chase.racc'
	end

	desc 'Clean up generated files and files output during debugging'
	task :clean do
		sh 'rm -f **/*.{output,rex.rb,tab.rb}'
	end

	desc 'Test the generated parser against the sample program'
	task :test do
		sh 'ruby -Ku *.tab.rb theories/trichotomy'
	end

end


desc 'Generate lexer with alex'
task :lexer do
	sh 'alex lexer.x'
end

desc 'Generate parser with happy'
task :parser do
	sh 'happy -i parser.y'
end

desc 'Compile using GHC'
task :compile do
	sh 'ghc -o chase lexer.hs parser.hs helpers.hs chase.hs main.hs'
	sh 'chmod u+x chase'
end

desc 'Clean up generated files and files output during debugging'
task :clean do
	sh 'rm -rf chase {lexer,parser}.hs a.out *.{hi,info,o} models'
end

desc 'Test the generated parser against the sample program'
task :test, [:theory] do |task,args|
	args.with_defaults :theory => 'trichotomy'
	sh "cat theories/#{args[:theory]} | ./chase"
end


namespace 'paper' do

	desc 'Generate PDF from latex document'
	task :pdf do
		Dir.chdir 'paper' do
			sh 'pdflatex paper.tex'
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
